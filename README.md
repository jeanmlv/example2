# example2

from pathlib import Path

import pandas as pd
import plotly.express as px
import streamlit as st

from src.ard_loader import (
    discover_ard_files,
    filter_ard_files_for_studies,
    load_ard,
    load_uploaded_ard,
    ordered_visits,
    prepare_variable_data,
    study_id_from_filename,
    variable_catalog,
)
from src.config import JNJ_RED, JNJ_RED_PALETTE


def _render_portfolio_summary(filtered: dict[str, pd.DataFrame]) -> None:
    s = filtered["01_STUDIES"]
    avail = filtered.get("02_DATA_AVAILABILITY", pd.DataFrame())
    ard = filtered.get("06_ARD", pd.DataFrame())

    c1, c2, c3, c4, c5 = st.columns(5)
    c1.metric("Studies", s["Study ID"].nunique() if "Study ID" in s else 0)
    c2.metric("Patients", f"{pd.to_numeric(s.get('Patients'), errors='coerce').sum():,.0f}" if "Patients" in s else "—")
    c3.metric("Videos", f"{pd.to_numeric(s.get('Videos'), errors='coerce').sum():,.0f}" if "Videos" in s else "—")

    available = (
        avail["Analysis-Ready-Dataset (ARD)"].astype(str).str.lower().eq("available").sum()
        if "Analysis-Ready-Dataset (ARD)" in avail
        else 0
    )
    c4.metric("ARD available", available)

    # Mean ARD coverage: same calculation as views/ard.py.
    # Excel percentages are typically loaded as fractions (e.g. 0.89 = 89%).
    from views.ard import calculate_mean_coverage

    avg = calculate_mean_coverage(ard)
    c5.metric(
        "Mean ARD coverage",
        f"{avg:.0f}%" if pd.notna(avg) else "—",
        help="Average ARD coverage for studies with coverage >= 1%.",
    )


def _render_portfolio_charts(filtered: dict[str, pd.DataFrame]) -> None:
    s = filtered["01_STUDIES"]
    left, right = st.columns([1, 1], gap="large")

    with left:
        st.markdown("### Portfolio by disease")
        if "Disease" in s and not s.empty:
            x = s["Disease"].fillna("Not specified").value_counts().reset_index()
            x.columns = ["Disease", "Studies"]
            fig = px.bar(
                x,
                x="Studies",
                y="Disease",
                orientation="h",
                text="Studies",
                color_discrete_sequence=[JNJ_RED],
            )
            fig.update_layout(
                height=330,
                margin=dict(l=5, r=10, t=15, b=10),
                yaxis_title="",
                xaxis_title="Studies",
                showlegend=False,
            )
            st.plotly_chart(fig, width="stretch")

    with right:
        st.markdown("### Trial status")
        if "Trial Status" in s and not s["Trial Status"].dropna().empty:
            x = s["Trial Status"].fillna("Not specified").astype(str).value_counts().reset_index()
            x.columns = ["Trial Status", "Count"]
            fig = px.pie(
                x,
                names="Trial Status",
                values="Count",
                hole=0.62,
                color_discrete_sequence=[JNJ_RED, "#F97066", "#FECACA", "#667085", "#98A2B3"],
            )
            fig.update_traces(textposition="inside", textinfo="percent")
            fig.update_layout(
                height=330,
                margin=dict(l=5, r=10, t=15, b=10),
                legend=dict(orientation="v", yanchor="middle", y=0.5, xanchor="left", x=1.02),
                legend_title_text="",
            )
            st.plotly_chart(fig, width="stretch")


def _render_numeric_explorer(variable_df: pd.DataFrame, variable: str, visits: list[str]) -> None:
    left, right = st.columns(2, gap="large")

    with left:
        st.markdown("#### Overall distribution")
        fig = px.histogram(variable_df, x="VALUE", nbins=30, labels={"VALUE": variable})
        fig.update_traces(marker_color=JNJ_RED)
        fig.update_layout(
            height=360,
            margin=dict(l=10, r=10, t=10, b=10),
            yaxis_title="Observations",
            showlegend=False,
        )
        st.plotly_chart(fig, width="stretch")

    with right:
        st.markdown("#### Distribution by visit")
        if "AVISIT" not in variable_df.columns or not visits:
            st.info("AVISIT is not available for this ARD variable.")
            return
        fig = px.box(
            variable_df,
            x="AVISIT",
            y="VALUE",
            points=False,
            category_orders={"AVISIT": visits},
            labels={"VALUE": variable, "AVISIT": "Visit"},
        )
        fig.update_traces(marker_color=JNJ_RED, line_color=JNJ_RED)
        fig.update_layout(height=360, margin=dict(l=10, r=10, t=10, b=10), showlegend=False)
        fig.update_xaxes(tickangle=-45)
        st.plotly_chart(fig, width="stretch")


def _render_categorical_explorer(variable_df: pd.DataFrame, variable: str, visits: list[str]) -> None:
    # Protect readability from high-cardinality AVALC variables.
    overall = variable_df["VALUE"].astype(str).value_counts().head(20).rename_axis("Value").reset_index(name="Count")
    left, right = st.columns(2, gap="large")

    with left:
        st.markdown("#### Overall distribution")
        fig = px.bar(overall, x="Count", y="Value", orientation="h", text="Count")
        fig.update_traces(marker_color=JNJ_RED)
        fig.update_layout(height=360, margin=dict(l=10, r=10, t=10, b=10), yaxis_title="", showlegend=False)
        st.plotly_chart(fig, width="stretch")

    with right:
        st.markdown("#### Distribution by visit")
        if "AVISIT" not in variable_df.columns or not visits:
            st.info("AVISIT is not available for this ARD variable.")
            return

        top_values = overall["Value"].astype(str).tolist()[:10]
        by_visit = variable_df[variable_df["VALUE"].astype(str).isin(top_values)].copy()
        by_visit["VALUE"] = by_visit["VALUE"].astype(str)
        counts = by_visit.groupby(["AVISIT", "VALUE"], dropna=False).size().reset_index(name="Count")
        fig = px.bar(
            counts,
            x="AVISIT",
            y="Count",
            color="VALUE",
            barmode="group",
            color_discrete_sequence=JNJ_RED_PALETTE,
            category_orders={"AVISIT": visits},
            labels={"AVISIT": "Visit", "VALUE": variable},
        )
        fig.update_layout(height=360, margin=dict(l=10, r=10, t=10, b=10), legend_title_text=variable)
        fig.update_xaxes(tickangle=-45)
        st.plotly_chart(fig, width="stretch")


def _render_variable_explorer(filtered: dict[str, pd.DataFrame]) -> None:
    st.markdown("---")
    st.markdown("### Variable Explorer")
    st.caption(
        "Upload an ARD CSV or use an ARD available in the project to explore "
        "PARAMCD variables overall and across visits. AVAL is treated as numeric "
        "and AVALC as categorical."
    )

    studies = filtered.get("01_STUDIES", pd.DataFrame())
    study_ids = studies["Study ID"].dropna().astype(str).tolist() if "Study ID" in studies else []

    # Store the uploaded ARD independently from the file_uploader widget.
    # Widget state can be cleared when another dashboard section is rendered.
    if "overview_ard_bytes" not in st.session_state:
        st.session_state["overview_ard_bytes"] = None
    if "overview_ard_name" not in st.session_state:
        st.session_state["overview_ard_name"] = None
    if "overview_ard_upload_version" not in st.session_state:
        st.session_state["overview_ard_upload_version"] = 0

    uploaded = st.file_uploader(
        "Upload ARD (.csv)",
        type=["csv"],
        key=f"overview_ard_upload_{st.session_state['overview_ard_upload_version']}",
        help="Upload an Analysis-Ready Dataset (ARD) CSV for the current session.",
    )

    if uploaded is not None:
        new_bytes = uploaded.getvalue()
        if (new_bytes != st.session_state["overview_ard_bytes"]
                or uploaded.name != st.session_state["overview_ard_name"]):
            st.session_state["overview_ard_bytes"] = new_bytes
            st.session_state["overview_ard_name"] = uploaded.name

    ard = None
    source_label = None

    if st.session_state["overview_ard_bytes"] is not None:
        col_info, col_remove = st.columns([5, 1])
        with col_info:
            st.caption(f"Active uploaded ARD: {st.session_state['overview_ard_name']}")
        with col_remove:
            if st.button("Remove ARD", key="overview_remove_ard"):
                st.session_state["overview_ard_bytes"] = None
                st.session_state["overview_ard_name"] = None
                st.session_state["overview_ard_upload_version"] += 1
                st.rerun()

        try:
            ard = load_uploaded_ard(st.session_state["overview_ard_bytes"])
            source_label = st.session_state["overview_ard_name"]
        except Exception as exc:
            st.error(f"The uploaded ARD could not be loaded: {exc}")
            return
    else:
        files = discover_ard_files()
        eligible_files = filter_ard_files_for_studies(files, study_ids) if files else []

        if eligible_files:
            file_labels = {}
            for path in eligible_files:
                sid = study_id_from_filename(path, study_ids)
                file_labels[path] = f"{sid} · {path.name}" if sid else path.name

            selected_path = st.selectbox(
                "Or use an ARD available in the project",
                options=eligible_files,
                format_func=lambda p: file_labels[p],
                key="overview_ard_file",
            )

            try:
                ard = load_ard(str(Path(selected_path)))
                source_label = file_labels[selected_path]
            except Exception as exc:
                st.error(f"The selected ARD could not be loaded: {exc}")
                return
        else:
            st.info("Upload an ARD CSV above to start exploring variables.")
            return

    catalog = variable_catalog(tuple(ard.columns))
    if catalog.empty:
        st.warning(
            "No columns matching PARAMCD_<parameter>_AVAL or "
            "PARAMCD_<parameter>_AVALC were found in this ARD."
        )
        return

    st.success(
        f"ARD loaded: {source_label} · {len(ard):,} rows × "
        f"{len(ard.columns):,} columns · {catalog['Variable'].nunique():,} PARAMCD variables"
    )

    search = st.text_input(
        "Search variable",
        placeholder="e.g. MAYO, SESTOT, UCEIS...",
        key="overview_variable_search",
    )
    choices = catalog
    if search.strip():
        mask = (
            catalog["Variable"].str.contains(search.strip(), case=False, na=False, regex=False)
            | catalog["Source"].str.contains(search.strip(), case=False, na=False, regex=False)
        )
        choices = catalog[mask]

    if choices.empty:
        st.info("No variable matches the current search.")
        return

    selected_label = st.selectbox(
        "Variable",
        choices["Label"].tolist(),
        key="overview_variable",
    )
    row = choices.loc[choices["Label"].eq(selected_label)].iloc[0]
    variable_df, kind = prepare_variable_data(ard, row["Column"], row["Value Type"])

    st.caption(
        f"Source: {row['Source']}  •  Value type: {row['Value Type']}  •  "
        f"Column: {row['Column']}"
    )

    if variable_df.empty:
        st.info("This variable has no non-missing values in the selected ARD.")
        return

    observations = len(variable_df)
    subjects = variable_df["USUBJID"].nunique() if "USUBJID" in variable_df else None
    visits = ordered_visits(variable_df)

    k1, k2, k3, k4, k5 = st.columns(5)

    k1.metric("Observations", f"{observations:,}")

    k2.metric(
        "Subjects",
        f"{subjects:,}" if subjects is not None else "—"
    )

    k3.metric(
        "Visits",
        f"{len(visits):,}" if visits else "—"
    )

    if kind == "numeric":
        k4.metric(
            "Mean",
            f"{variable_df['VALUE'].mean():.2f}"
        )

        k5.metric(
            "Median",
            f"{variable_df['VALUE'].median():.2f}"
        )

        _render_numeric_explorer(
        variable_df,
        row["Variable"],
        visits
        )

    else:
        k4.metric(
            "Categories",
            f"{variable_df['VALUE'].nunique():,}"
        )

        k5.metric(
            "Mode",
            (
             str(variable_df["VALUE"].mode().iloc[0])
                if not variable_df["VALUE"].mode().empty
                else "—"
            )
        )

        _render_categorical_explorer(
            variable_df,
            row["Variable"],
            visits
        )

    export_df = variable_df.copy()
    export_df.insert(0, "PARAMCD", row["Variable"])
    export_df.insert(1, "VALUE_TYPE", row["Value Type"])
    export_df.insert(2, "SOURCE_COLUMN", row["Column"])


def render(filtered: dict[str, pd.DataFrame], data: dict[str, pd.DataFrame]) -> None:
    _render_portfolio_summary(filtered)
    _render_portfolio_charts(filtered)
    _render_variable_explorer(filtered)
