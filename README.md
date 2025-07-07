# Previsió de professorat infantil i primària

## Scripts

### "0. Funcions.R" i "0. Libs.r"

Funcions i llibreries necessàries

### "1. Entrades borsa.R"

Entrades netes a la borsa, eporta base de dades *entrades_netes_inf_prim_19_24*

Entrades i sortides per especialitat, en cas que es demani més d'una especialitat a la borsa, s'assigna especialitat seguint la distribució d'aquells que van demanar les mateixes especialitats i que van entrar en plantilla.

### "1.1 Entrades borsa predicció.R"

Predicció d'entrades netes a la borsa fins al 2032-2033 segons els 3 escenaris demogràfics

### "2. Entrades panell.R"

A partir del nombre de sortides per curs i especialitat, generar el mateix nombre d'entrades al panell (ficticies) seguint una distribució similar a aquells que entren en el panell (individus reals): manteniment d'estoc de professorat en plantilla

### "Naturalesa alumnes"

Assignar al "panell_alumnes.dta" la naturalesa del centre on estan matriculats (públic/privat)
