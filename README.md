# example2

import streamlit as st
import pandas as pd
import plotly.express as px
import re
import streamlit.components.v1 as components

try:
    import pygwalker as pyg
    PYGWALKER_AVAILABLE = True
except ImportError:
    PYGWALKER_AVAILABLE = False


st.set_page_config(page_title="ARD Explorer", layout="wide")
st.title("ARD Explorer - Clinical Dashboard")

MAX_LONG_COLUMNS = 50
MAX_WIDE_DISPLAY_COLUMNS = 80
ADSL_FILTER_SUFFIXES = ("_ARM", "_ARMCD", "_ACTARM", "_ACTARMCD", "_AGE", "_SEX", "_COUNTRY", "_RACE", "_ETHNIC")


def get_adsl_filter_cols(df):
    return [
        col for col in df.columns
        if col.upper().startswith("ADSL")
        and col.upper().endswith(ADSL_FILTER_SUFFIXES)
    ]


def fill_adsl_by_subject(df, adsl_cols):
    df = df.copy()

    for col in adsl_cols:
        df[col] = (
            df.groupby("USUBJID")[col]
            .transform(lambda x: x.dropna().iloc[0] if x.dropna().shape[0] > 0 else pd.NA)
        )

    return df


def parse_ard_column(col):
    pattern = r"^(.*?)_PARAMCD_(.*?)_(AVALC|AVAL)$"
    match = re.match(pattern, col)

    if match:
        return match.group(1), match.group(2), match.group(3)

    return None, None, None


def detect_subject_column(df):
    priority_cols = [
        "ADSL_SUBJID",
        "ADSL_SAS7BDAT_SUBJID",
        "ADSL_XPT_SUBJID"
    ]

    for col in priority_cols:
        if col in df.columns:
            return col

    candidates = [
        col for col in df.columns
        if col.upper().startswith("ADSL")
        and col.upper().endswith("_SUBJID")
        and "PSUBJID" not in col.upper()
    ]

    if candidates:
        return candidates[0]

    if "USUBJID" in df.columns:
        return "USUBJID"

    return None


@st.cache_data
def load_file(uploaded_file):
    paramcd_dict = None

    if uploaded_file.name.lower().endswith(".csv"):
        df = pd.read_csv(uploaded_file)
    else:
        xls = pd.ExcelFile(uploaded_file)
        sheet_names = xls.sheet_names

        if "ARD" in sheet_names:
            df = pd.read_excel(uploaded_file, sheet_name="ARD")
        else:
            df = pd.read_excel(uploaded_file, sheet_name=sheet_names[0])

        if "PARAMCD_DICT" in sheet_names:
            paramcd_dict = pd.read_excel(uploaded_file, sheet_name="PARAMCD_DICT")

    df.columns = df.columns.str.strip()

    if paramcd_dict is not None:
        paramcd_dict.columns = paramcd_dict.columns.str.strip()
        paramcd_dict = paramcd_dict.loc[:, ~paramcd_dict.columns.str.contains("^Unnamed")]

    return df, paramcd_dict


@st.cache_data
def build_param_map(df):
    param_cols = []

    for col in df.columns:
        source, paramcd, value_type = parse_ard_column(col)

        if paramcd is not None:
            param_cols.append({
                "column": col,
                "source": source,
                "paramcd": paramcd,
                "value_type": value_type
            })

    return pd.DataFrame(param_cols)


def safe_numeric(series):
    cleaned = (
        series.astype(str)
        .str.strip()
        .str.replace(",", ".", regex=False)
    )

    cleaned = cleaned.where(
        cleaned.str.match(r"^-?\d+(\.\d+)?$", na=False),
        None
    )

    return pd.to_numeric(cleaned, errors="coerce")


@st.cache_data
def convert_wide_to_long(filtered_df, filtered_map, adsl_cols=None):
    if adsl_cols is None:
        adsl_cols = []

    base_cols = ["USUBJID", "AVISIT", "AVISITN"]

    if "AVISIT_ORDER" in filtered_df.columns:
        base_cols.append("AVISIT_ORDER")

    base_cols = base_cols + adsl_cols
    base_cols = [col for col in dict.fromkeys(base_cols) if col in filtered_df.columns]

    long_parts = []

    for _, row in filtered_map.iterrows():
        col = row["column"]

        temp = filtered_df[base_cols + [col]].copy()
        temp = temp.rename(columns={col: "VALUE"})

        temp["SOURCE"] = row["source"]
        temp["PARAMCD"] = row["paramcd"]
        temp["VALUE_TYPE"] = row["value_type"]
        temp["ORIGINAL_COLUMN"] = col

        temp = temp[temp["VALUE"].notna()]

        if not temp.empty:
            long_parts.append(temp)

    if not long_parts:
        return pd.DataFrame()

    long_df = pd.concat(long_parts, ignore_index=True)
    long_df["VALUE_NUMERIC"] = safe_numeric(long_df["VALUE"])

    return long_df


def compute_wide_metrics(filtered_df, selected_columns, visit_col):
    if not selected_columns:
        return 0, 0, 0, 0

    row_has_data = pd.Series(False, index=filtered_df.index)
    records = 0

    for col in selected_columns:
        non_missing = filtered_df[col].notna()
        row_has_data = row_has_data | non_missing
        records += int(non_missing.sum())

    filtered_rows = filtered_df.loc[row_has_data]

    filtered_subjects = filtered_rows["USUBJID"].nunique()

    visit_subjects = (
        filtered_rows
        .groupby(visit_col)["USUBJID"]
        .nunique()
    )

    max_visit_subjects = int(visit_subjects.max()) if not visit_subjects.empty else 0
    filtered_visits = filtered_rows[visit_col].nunique()

    return filtered_subjects, records, max_visit_subjects, filtered_visits


def build_visit_chart(filtered_df, selected_columns, visit_col):
    if not selected_columns:
        return pd.DataFrame(columns=[visit_col, "AVISITN", "Unique Subjects"])

    row_has_data = pd.Series(False, index=filtered_df.index)

    for col in selected_columns:
        row_has_data = row_has_data | filtered_df[col].notna()

    chart_df = (
        filtered_df.loc[row_has_data]
        .groupby([visit_col, "AVISITN"])["USUBJID"]
        .nunique()
        .reset_index(name="Unique Subjects")
    )

    chart_df["AVISITN_NUM"] = pd.to_numeric(chart_df["AVISITN"], errors="coerce")
    chart_df = chart_df.sort_values("AVISITN_NUM")

    return chart_df


def build_param_records_chart(filtered_df, filtered_map):
    rows = []

    for _, row in filtered_map.iterrows():
        col = row["column"]

        rows.append({
            "SOURCE": row["source"],
            "PARAMCD": row["paramcd"],
            "VALUE_TYPE": row["value_type"],
            "Records": int(filtered_df[col].notna().sum())
        })

    if not rows:
        return pd.DataFrame(columns=["PARAMCD", "Records"])

    return (
        pd.DataFrame(rows)
        .groupby("PARAMCD")["Records"]
        .sum()
        .reset_index()
        .sort_values("Records", ascending=False)
        .head(30)
    )




def aggregate_chart_data(df, x_col, y_col, color_col=None, agg_func="mean"):
    group_cols = [x_col]

    if color_col and color_col != "None":
        group_cols.append(color_col)

    work_df = df.copy()

    if agg_func == "count":
        chart_df = (
            work_df
            .groupby(group_cols, dropna=False)
            .size()
            .reset_index(name="COUNT")
        )
        y_out = "COUNT"
    else:
        work_df[y_col] = pd.to_numeric(work_df[y_col], errors="coerce")
        work_df = work_df.dropna(subset=[y_col])

        chart_df = (
            work_df
            .groupby(group_cols, dropna=False)[y_col]
            .agg(agg_func)
            .reset_index()
        )
        y_out = y_col

    if "AVISITN_ORDER" in chart_df.columns:
        chart_df = chart_df.sort_values("AVISITN_ORDER")

    return chart_df, y_out


def apply_visit_order(fig, df, x_col):
    if x_col == "AVISIT_ORDER_SORTED" and x_col in df.columns:
        order = df.sort_values("AVISITN_ORDER")[x_col].dropna().astype(str).unique().tolist()
        fig.update_xaxes(categoryorder="array", categoryarray=order)
    return fig

def build_paramcd_summary_table(filtered_df, filtered_map):
    rows = []

    for _, row in filtered_map.iterrows():
        col = row["column"]
        non_missing_mask = filtered_df[col].notna()

        rows.append({
            "SOURCE": row["source"],
            "PARAMCD": row["paramcd"],
            "VALUE_TYPE": row["value_type"],
            "records": int(non_missing_mask.sum()),
            "unique_subjects": filtered_df.loc[non_missing_mask, "USUBJID"].nunique()
        })

    return pd.DataFrame(rows)


uploaded_file = st.file_uploader(
    "Upload your ARD file",
    type=["csv", "xlsx"]
)

if uploaded_file is not None:

    df, external_paramcd_dict = load_file(uploaded_file)

    required_cols = ["USUBJID", "AVISIT", "AVISITN"]
    missing = [col for col in required_cols if col not in df.columns]

    if missing:
        st.error(f"Missing required columns: {missing}")
        st.stop()

    subject_col = detect_subject_column(df)

    if subject_col is None:
        st.error("No subject column found. Expected USUBJID or an ADSL column ending with SUBJID.")
        st.stop()

    study_subjects = df[subject_col].nunique()

    visit_col = "AVISIT_ORDER" if "AVISIT_ORDER" in df.columns else "AVISIT"

    df[visit_col] = df[visit_col].astype(str)
    df["AVISITN"] = pd.to_numeric(df["AVISITN"], errors="coerce")

    param_map = build_param_map(df)

    if param_map.empty:
        st.error(
            "No PARAMCD columns found. Check if your columns contain '_PARAMCD_' and end with '_AVAL' or '_AVALC'."
        )
        st.stop()

    adsl_filter_cols = get_adsl_filter_cols(df)
    df = fill_adsl_by_subject(df, adsl_filter_cols)

    st.sidebar.header("Filters")

    selected_source = st.sidebar.multiselect(
        "Source dataset",
        sorted(param_map["source"].unique())
    )

    temp_map = param_map.copy()

    if selected_source:
        temp_map = temp_map[temp_map["source"].isin(selected_source)]

    selected_paramcd = st.sidebar.multiselect(
        "PARAMCD",
        sorted(temp_map["paramcd"].unique())
    )

    selected_value_type = st.sidebar.multiselect(
        "Value type",
        sorted(temp_map["value_type"].unique()),
        default=["AVAL"] if "AVAL" in temp_map["value_type"].unique() else []
    )

    visit_options_df = (
        df[[visit_col, "AVISITN"]]
        .dropna(subset=[visit_col])
        .drop_duplicates()
        .sort_values("AVISITN")
    )

    selected_avisit = st.sidebar.multiselect(
        "Visit",
        visit_options_df[visit_col].tolist()
    )

    adsl_selected_filters = {}

    if adsl_filter_cols:
        with st.sidebar.expander("ADSL Filters", expanded=False):
            for col in adsl_filter_cols:

                if col.upper().endswith("_AGE"):
                    age_values = pd.to_numeric(df[col], errors="coerce").dropna()

                    if not age_values.empty:
                        min_age = int(age_values.min())
                        max_age = int(age_values.max())

                        selected_age = st.slider(
                            col,
                            min_value=min_age,
                            max_value=max_age,
                            value=(min_age, max_age)
                        )

                        adsl_selected_filters[col] = selected_age

                else:
                    options = sorted(df[col].dropna().astype(str).unique())

                    selected_values = st.multiselect(
                        col,
                        options
                    )

                    if selected_values:
                        adsl_selected_filters[col] = selected_values

    filtered_map = temp_map.copy()

    if selected_paramcd:
        filtered_map = filtered_map[filtered_map["paramcd"].isin(selected_paramcd)]

    if selected_value_type:
        filtered_map = filtered_map[filtered_map["value_type"].isin(selected_value_type)]

    selected_columns = filtered_map["column"].tolist()

    filtered_df = df.copy()
    filtered_df[visit_col] = filtered_df[visit_col].astype(str)

    if selected_avisit:
        filtered_df = filtered_df[filtered_df[visit_col].isin(selected_avisit)]

    for col, selected_filter in adsl_selected_filters.items():

        if col.upper().endswith("_AGE"):
            min_age, max_age = selected_filter
            age_numeric = pd.to_numeric(filtered_df[col], errors="coerce")

            filtered_df = filtered_df[
                (age_numeric >= min_age) &
                (age_numeric <= max_age)
            ]

        else:
            filtered_df = filtered_df[
                filtered_df[col].astype(str).isin(selected_filter)
            ]

    filtered_subjects, records, max_visit_subjects, filtered_visits = compute_wide_metrics(
        filtered_df,
        selected_columns,
        visit_col
    )

    can_generate_long = (
        len(selected_paramcd) > 0
        and len(selected_columns) > 0
        and len(selected_columns) <= MAX_LONG_COLUMNS
    )

    tab1, tab2, tab3, tab4 = st.tabs([
        "Clinical Dashboard",
        "Filtered Data",
        "Visual Explorer",
        "PARAMCD Dictionary"
    ])

    with tab1:
        col1, col2, col3, col4, col5, col6, col7 = st.columns(7)

        col1.metric("Study Subjects", study_subjects)
        col2.metric("Filtered Subjects", filtered_subjects)
        col3.metric("Max Visit Subjects", max_visit_subjects)
        col4.metric("Records", records)
        col5.metric("PARAMCDs", filtered_map["paramcd"].nunique())
        col6.metric("Visits", filtered_visits)
        col7.metric("Sources", filtered_map["source"].nunique())

        st.divider()

        c1, c2 = st.columns(2)

        with c1:
            st.subheader("Unique Subjects by Visit")

            visit_chart = build_visit_chart(
                filtered_df,
                selected_columns,
                visit_col
            )

            fig_visit = px.bar(
                visit_chart,
                x=visit_col,
                y="Unique Subjects",
                title="Unique Subjects by Visit"
            )

            if not visit_chart.empty:
                fig_visit.update_xaxes(
                    categoryorder="array",
                    categoryarray=visit_chart[visit_col].tolist()
                )

            st.plotly_chart(fig_visit, use_container_width=True)

        with c2:
            st.subheader("Records by PARAMCD")

            param_chart = build_param_records_chart(filtered_df, filtered_map)

            fig_param = px.bar(
                param_chart,
                x="PARAMCD",
                y="Records",
                title="Top PARAMCDs by Number of Records"
            )

            st.plotly_chart(fig_param, use_container_width=True)

        st.subheader("PARAMCD Summary Table")

        paramcd_summary = build_paramcd_summary_table(filtered_df, filtered_map)
        st.dataframe(paramcd_summary, use_container_width=True)

        if can_generate_long:
            filtered_long_for_chart = convert_wide_to_long(
                filtered_df,
                filtered_map,
                adsl_filter_cols
            )

            if not filtered_long_for_chart.empty:
                numeric_data = filtered_long_for_chart.dropna(subset=["VALUE_NUMERIC"])

                if not numeric_data.empty:
                    st.subheader("Numeric VALUE Distribution")

                    fig_hist = px.histogram(
                        numeric_data,
                        x="VALUE_NUMERIC",
                        color="PARAMCD",
                        title="Distribution of Numeric Values"
                    )

                    st.plotly_chart(fig_hist, use_container_width=True)

    with tab2:
        st.subheader("Filtered ARD - Wide Format")

        base_cols = ["USUBJID", "AVISIT", "AVISITN"]

        if "AVISIT_ORDER" in filtered_df.columns:
            base_cols.append("AVISIT_ORDER")

        base_cols = base_cols + adsl_filter_cols
        base_cols = [col for col in dict.fromkeys(base_cols) if col in filtered_df.columns]

        if len(selected_columns) > MAX_WIDE_DISPLAY_COLUMNS:
            st.warning(
                f"{len(selected_columns)} PARAMCD columns are selected. "
                f"Please filter by Source Dataset or PARAMCD to display up to {MAX_WIDE_DISPLAY_COLUMNS} columns."
            )
            result_df = filtered_df[base_cols].copy()
        else:
            result_cols = base_cols + selected_columns
            result_df = filtered_df[result_cols].copy()

            if selected_columns:
                row_has_data = pd.Series(False, index=result_df.index)

                for col in selected_columns:
                    row_has_data = row_has_data | result_df[col].notna()

                result_df = result_df[row_has_data]

        st.dataframe(result_df, use_container_width=True)

        csv_wide = result_df.to_csv(index=False).encode("utf-8")

        st.download_button(
            "Download filtered wide ARD",
            csv_wide,
            "filtered_ard_wide.csv",
            "text/csv"
        )

        st.divider()

        st.subheader("Filtered ARD - Long Format")

        if can_generate_long:
            filtered_long = convert_wide_to_long(
                filtered_df,
                filtered_map,
                adsl_filter_cols
            )

            st.dataframe(filtered_long, use_container_width=True)

            csv_long = filtered_long.to_csv(index=False).encode("utf-8")

            st.download_button(
                "Download filtered long ARD",
                csv_long,
                "filtered_ard_long.csv",
                "text/csv"
            )
        else:
            st.warning(
                "Long format is generated only after selecting at least one PARAMCD "
                f"and up to {MAX_LONG_COLUMNS} selected PARAMCD columns."
            )

    with tab3:
        st.subheader("Visual Explorer")

        if not selected_paramcd:
            st.warning("Please select at least one PARAMCD to use the Visual Explorer.")

        elif len(selected_columns) > MAX_LONG_COLUMNS:
            st.warning(
                f"Too many PARAMCD columns selected ({len(selected_columns)}). "
                f"Please select fewer PARAMCDs, ideally one at a time."
            )

        else:
            filtered_long = convert_wide_to_long(
                filtered_df,
                filtered_map,
                adsl_filter_cols
            )

            if filtered_long.empty:
                st.warning("No data available for Visual Explorer with the current filters.")

            else:
                explorer_df = filtered_long.copy()

                explorer_df["AVISITN_ORDER"] = pd.to_numeric(
                    explorer_df["AVISITN"],
                    errors="coerce"
                )

                explorer_df["AVISIT_ORDER_SORTED"] = (
                    explorer_df["AVISITN_ORDER"]
                    .fillna(999)
                    .astype(int)
                    .astype(str)
                    .str.zfill(3)
                    + "-"
                    + explorer_df["AVISIT"].astype(str)
                )

                explorer_df = explorer_df.sort_values("AVISITN_ORDER")

                max_rows = st.slider(
                    "Maximum rows sent to Visual Explorer",
                    min_value=1000,
                    max_value=50000,
                    value=10000,
                    step=1000
                )

                explorer_df = explorer_df.head(max_rows)
                explorer_df["VALUE_TEXT"] = explorer_df["VALUE"].astype(str)
                explorer_df = explorer_df.drop(columns=["VALUE"], errors="ignore")

                explorer_mode = st.radio(
                    "Explorer mode",
                    ["Clinical Chart Builder", "PyGWalker Ad Hoc Explorer"],
                    horizontal=True
                )

                if explorer_mode == "Clinical Chart Builder":
                    st.markdown("Build controlled clinical charts by selecting chart type, axes, grouping, and aggregation.")

                    numeric_cols = explorer_df.select_dtypes(include="number").columns.tolist()
                    all_cols = explorer_df.columns.tolist()
                    default_x = "AVISIT_ORDER_SORTED" if "AVISIT_ORDER_SORTED" in all_cols else all_cols[0]
                    default_y = "VALUE_NUMERIC" if "VALUE_NUMERIC" in numeric_cols else (numeric_cols[0] if numeric_cols else all_cols[0])
                    default_color = "ADSL_ARM" if "ADSL_ARM" in all_cols else "PARAMCD"

                    c1, c2, c3, c4 = st.columns(4)

                    with c1:
                        chart_type = st.selectbox(
                            "Chart type",
                            [
                                "Line - Mean/Median by Visit",
                                "Bar - Aggregated Summary",
                                "Boxplot - Distribution",
                                "Scatter - Individual Values",
                                "Histogram - Distribution",
                                "Heatmap - Mean Value"
                            ]
                        )

                    with c2:
                        x_col = st.selectbox(
                            "X-Axis",
                            all_cols,
                            index=all_cols.index(default_x)
                        )

                    with c3:
                        y_col = st.selectbox(
                            "Y-Axis / Measure",
                            numeric_cols if numeric_cols else all_cols,
                            index=(numeric_cols if numeric_cols else all_cols).index(default_y)
                        )

                    with c4:
                        color_options = ["None"] + all_cols
                        color_col = st.selectbox(
                            "Color / Group",
                            color_options,
                            index=color_options.index(default_color) if default_color in color_options else 0
                        )

                    c5, c6, c7 = st.columns(3)

                    with c5:
                        agg_func = st.selectbox(
                            "Aggregation",
                            ["mean", "median", "sum", "count", "min", "max"]
                        )

                    with c6:
                        facet_col = st.selectbox(
                            "Facet by",
                            ["None"] + all_cols,
                            index=0
                        )

                    with c7:
                        show_points = st.checkbox("Show points/markers", value=True)

                    plot_color = None if color_col == "None" else color_col
                    plot_facet = None if facet_col == "None" else facet_col

                    try:
                        if chart_type.startswith("Line"):
                            chart_df, y_out = aggregate_chart_data(
                                explorer_df,
                                x_col=x_col,
                                y_col=y_col,
                                color_col=plot_color,
                                agg_func=agg_func
                            )

                            fig = px.line(
                                chart_df,
                                x=x_col,
                                y=y_out,
                                color=plot_color,
                                facet_col=plot_facet,
                                markers=show_points,
                                title=f"{agg_func.upper()} of {y_col} by {x_col}"
                            )
                            fig = apply_visit_order(fig, explorer_df, x_col)

                        elif chart_type.startswith("Bar"):
                            chart_df, y_out = aggregate_chart_data(
                                explorer_df,
                                x_col=x_col,
                                y_col=y_col,
                                color_col=plot_color,
                                agg_func=agg_func
                            )

                            fig = px.bar(
                                chart_df,
                                x=x_col,
                                y=y_out,
                                color=plot_color,
                                facet_col=plot_facet,
                                barmode="group",
                                title=f"{agg_func.upper()} of {y_col} by {x_col}"
                            )
                            fig = apply_visit_order(fig, explorer_df, x_col)

                        elif chart_type.startswith("Boxplot"):
                            box_df = explorer_df.dropna(subset=[y_col]).copy()
                            fig = px.box(
                                box_df,
                                x=x_col,
                                y=y_col,
                                color=plot_color,
                                facet_col=plot_facet,
                                points="outliers",
                                title=f"Distribution of {y_col} by {x_col}"
                            )
                            fig = apply_visit_order(fig, explorer_df, x_col)

                        elif chart_type.startswith("Scatter"):
                            scatter_df = explorer_df.dropna(subset=[y_col]).copy()
                            hover_cols = [col for col in ["USUBJID", "PARAMCD", "SOURCE", "AVISIT"] if col in scatter_df.columns]
                            fig = px.scatter(
                                scatter_df,
                                x=x_col,
                                y=y_col,
                                color=plot_color,
                                facet_col=plot_facet,
                                hover_data=hover_cols,
                                title=f"Individual {y_col} by {x_col}"
                            )
                            fig = apply_visit_order(fig, explorer_df, x_col)

                        elif chart_type.startswith("Histogram"):
                            hist_df = explorer_df.dropna(subset=[y_col]).copy()
                            fig = px.histogram(
                                hist_df,
                                x=y_col,
                                color=plot_color,
                                facet_col=plot_facet,
                                marginal="box",
                                title=f"Distribution of {y_col}"
                            )

                        elif chart_type.startswith("Heatmap"):
                            heatmap_y = st.selectbox(
                                "Heatmap Y-Axis",
                                all_cols,
                                index=all_cols.index(default_color) if default_color in all_cols else 0
                            )

                            heat_df = explorer_df.dropna(subset=[y_col]).copy()
                            heat_df[y_col] = pd.to_numeric(heat_df[y_col], errors="coerce")
                            heat_df = (
                                heat_df
                                .groupby([x_col, heatmap_y], dropna=False)[y_col]
                                .mean()
                                .reset_index()
                            )

                            fig = px.density_heatmap(
                                heat_df,
                                x=x_col,
                                y=heatmap_y,
                                z=y_col,
                                histfunc="avg",
                                title=f"Mean {y_col} heatmap by {x_col} and {heatmap_y}"
                            )
                            fig = apply_visit_order(fig, explorer_df, x_col)

                        fig.update_layout(height=650)
                        st.plotly_chart(fig, use_container_width=True)

                        with st.expander("Chart data preview"):
                            if "chart_df" in locals():
                                st.dataframe(chart_df.head(500), use_container_width=True)
                            else:
                                st.dataframe(explorer_df.head(500), use_container_width=True)

                    except Exception as e:
                        st.error("The selected chart could not be generated with the current fields.")
                        st.write(str(e))

                else:
                    if not PYGWALKER_AVAILABLE:
                        st.warning("PyGWalker is not installed. Run: pip install pygwalker")
                    else:
                        try:
                            pyg_html = pyg.to_html(explorer_df)
                            components.html(pyg_html, height=900, scrolling=True)

                        except Exception as e:
                            st.error("Visual Explorer could not be loaded due to mixed value types.")
                            st.write(str(e))
                            st.dataframe(explorer_df.head(100), use_container_width=True)

    with tab4:
        st.subheader("PARAMCD Dictionary")

        if external_paramcd_dict is not None:
            dict_df = external_paramcd_dict.copy()
            st.dataframe(dict_df, use_container_width=True)

            csv_dict = dict_df.to_csv(index=False).encode("utf-8")

            st.download_button(
                "Download PARAMCD dictionary",
                csv_dict,
                "paramcd_dictionary.csv",
                "text/csv"
            )

        else:
            st.dataframe(filtered_map, use_container_width=True)

            csv_map = filtered_map.to_csv(index=False).encode("utf-8")

            st.download_button(
                "Download generated PARAMCD dictionary",
                csv_map,
                "paramcd_dictionary_generated.csv",
                "text/csv"
            )

else:
    st.info("Upload an ARD CSV or XLSX file to start.")
