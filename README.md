# Desafío Ministerio de Educación de Chile

Se pretende obtener una distribución optima de subsidios a la educación para evitar el abandono escolar, en el contexto de la pandemia (2020-2022)

Se busca distribuir USD 1.000.000 entre los estudiantes que muestran la mayor probabilidad a abandonar el colegio.

En este contexto, la validación final se hará contra los datos del ministerio _a posteriori_ (resultados del 2022)

NOTA: Se presupone la disponibilidad de la base previa de del ministerio en 
- "/desafio_ministerio/chile_2014_2020.csv.gz"
- "../desafio_ministerio/chile_2021.csv.gz"

(se puede modificar la ruta desde el config_desafio.yml)

La distribución sugerida de presupuesto se encuentra en el archivo "distribucion_presupuesto_2022.csv"

## Asignación del presupuesto

### Cálculo de probabilidades de abandono

Se utilizó un modelo predictivo para estimar la probabilidad de que cada alumno de la base 2021 abandone sus estudios durante 2022. Para ello, se aprovecha la información histórica de los años anteriores.
Luego, esas probabilidades se ajustan utilizando los resultados históricos reales. Este paso, llamado calibración, busca que las probabilidades estimadas sean más representativas de lo que efectivamente ocurrió.

### Asignación por alumno

El desafío cuenta con un presupuesto total de USD 1.000.000.

La distribución se realiza mediante un procedimiento de optimización que calcula cuánto asignar a cada alumno para obtener el mayor beneficio esperado posible, sin superar el presupuesto disponible.
Para tomar esta decisión se considera:

    - La probabilidad estimada de abandono de cada alumno.
    - El monto de dinero que se le asignaría.
    - El beneficio esperado de esa asignación.
    
El criterio supone que una mayor inversión puede generar un mayor beneficio, pero que ese beneficio adicional va disminuyendo a medida que aumenta el monto destinado al mismo alumno.
Por eso, no se asigna necesariamente la misma cantidad a todos ni se concentra todo el dinero en quienes tienen la probabilidad más alta. 
El criterio busca priorizar a los alumnos que presentan mayor riesgo de abandono y, al mismo tiempo, considerar que los recursos destinados a una misma persona tienen un rendimiento decreciente.
De esta manera, se busca utilizar el presupuesto de forma eficiente y maximizar el beneficio esperado de la intervención.

El resultado obtenido se muestra en la siguiente tabla:

### *RESUMEN DE LA DISTRIBUCION DEL PRESUPUESTO*


|                            concepto|       valor|        
|--------------------------------:|----------------:|
|                 Alumnos evaluados| 2775410|
|      Presupuesto disponible (USD)| 1000000|
|        Presupuesto asignado (USD)| 1000000|
|           Alumnos con asignación|6661000|
| Asignación media por alumno (USD)| 360.31
|           Asignación máxima (USD)| 442|
|         Beneficio esperado total| 604,36|
