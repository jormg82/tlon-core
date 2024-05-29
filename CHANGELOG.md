# Changelog

### [Mark 1] - 2024/05/20
Entrada:
Un fichero con una expresion Core simplificado, que solo incluye:
- Literales
- Abstracciones con un parametro
- Aplicacion
- let no recursivo con una sola definicion

Ejecución:
- El scanner lee el fichero y produce una lista de tokens
- El parser lee los tokens y produce una expresion Core
- El compilador traduce la expresion Core a instrucciones MVT
- El evaluador ejecuta la lista de instrucciones en MVT

Salida:
Un log de los estados de la maquina hasta el estado final.

### [Mark 2] - 2024/05/23
- Lambdas multiparametro en Core.
- Multiples definiciones en Core
- Mejoras en esquemas de compilacion.
