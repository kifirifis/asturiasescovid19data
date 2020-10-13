# Datos actualizados Asturias

El csv que se actualiza es *output.csv* dentro de la carpeta *data*.

Los datos de hoy se imputan al día previo.

En las variables que computan diferencias de acumulados se toma el último valor no NA para realizar la diferencia.

Este proyecto está dentro de otro más grande que es [montera34/escovid19data](https://github.com/montera34/escovid19data) y [numeroteca](https://twitter.com/numeroteca?ref_src=twsrc%5Egoogle%7Ctwcamp%5Eserp%7Ctwgr%5Eauthor).

### Variables

- PCR

cases_accumulated_pcr = X

$pcr = X_t - X_{t-1}; X_{t-1} \neq NA$
