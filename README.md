# Base de datos Observatorio Ciudadano COVID-19 Nicaragua

Este repositorio contiene datos extraídos de los reportes del [Observatorio Ciudadano COVID-19 Nicaragaua](https://observatorioni.org). Lamentablemente, en la página web del observatorio no se encuentra un archivo de descargable de sus datos. 

## Información en la base de datos

Actualmente, los datos en este repositorio contienen información del 18 de marzo al 27 de mayo (último día disponible). Los casos presentados incluyen información proporcionada por el Ministerio de Salud de Nicaragua (MINSA). En caso de requerir información única sobre los casos del MINSA, sugiero utilizar la información de la [European Centre for Disease Prevention and Control](https://www.ecdc.europa.eu/). De igual forma pueden usar el paquete [covdata](https://kjhealy.github.io/covdata/) en R, el cual contiene información de número de casos y muertes oficiales.

### Variables

Las siguientes variables se encuentran en este base de datos: 

* `date`: Fecha.
* `cases`: Acumulado de casos sospechosos COVID-19 reportados por el Observatorio Ciudadano.
* `deaths`: Acumulado de muertes sospechosas COVID-19 reportados por el Observatorio Ciudadano.
* `cases_minsa`: Acumulado de casos COVID-19 reportados por el MINSA.
* `deaths_minsa`: Acumulado de muertes COVID-19 reportados por el MINSA.

## Caveats

Los datos presentados por el Observatorio Ciudadano no representan los datos oficiales de Nicaragua. Su función es informar con mayor rigurosidad la presencia de COVID-19 dada la limitaciones de los datos del MINSA. 

> Notas: Trataré de actualizar los datos a medidas que salgan los reportes del Observatorio, y, de igual forma, incluir muertes sospechosas por COVID-19.

## Comentarios y sugerencias

Para realizar comentarios o sugerencias sobre la base de datos puedes abrir un issue en este repositorio: [https://github.com/rrmaximiliano/observatorio-covid-nicaragua/issues](https://github.com/rrmaximiliano/observatorio-covid-nicaragua/issues)
