# example2

1. FILE_COMPARISON

Essa é uma visão geral dos arquivos antes do merge.

Coluna	Significado
FILE	nome do arquivo
ROWS	quantidade de linhas na aba ARD
COLUMNS	quantidade de colunas
SUBJECTS	número de USUBJIDs
VISITS	número de AVISIT diferentes
UNIQUE_KEYS	quantidade de combinações únicas da chave (USUBJID+AVISIT...)
NULL_KEY_ROWS	linhas cuja chave possui NA
COLUMNS_ONLY_IN_THIS_FILE	colunas exclusivas daquele arquivo
MISSING_FROM_GLOBAL_COLUMN_UNION	colunas que existem em outros arquivos mas não nesse

No seu caso:

Arquivo	Subjects	Visits
wk12	398	35
wk28	398	19
wk78	398	59

Isso já mostra uma informação interessante:

os três arquivos possuem exatamente os mesmos sujeitos
o WK78 possui muito mais visitas
o WK12 possui muito mais variáveis

Ou seja:

o merge faz sentido.

2. VALUE_CONFLICTS

Essa é provavelmente a aba mais importante.

Ela lista TODAS as células em que existia informação diferente.

Por exemplo

USUBJID = XXXXX

COLUMN = ADADT_TRT02A

arquivo1

Placebo

arquivo2

Not treated

O script registra:

ACCUMULATED_VALUE

Placebo

INCOMING_VALUE

Not treated

MERGED_VALUE

Placebo | Not treated

Ou seja

ele nunca perdeu informação.

Isso é excelente.

Mas eu mudaria essa aba.

Hoje ela possui

ACCUMULATED_VALUE

INCOMING_FILE

INCOMING_VALUE

MERGED_VALUE

Eu colocaria também

SOURCE_FILE_1

SOURCE_FILE_2

para ficar muito mais fácil de auditar.

3. SOURCE_FILES

Essa aba simplesmente registra a ordem em que o merge ocorreu.

No seu caso

1 wk12

2 wk28

3 wk78

Isso é importante porque quando houver conflito você sabe qual arquivo foi lido primeiro.
