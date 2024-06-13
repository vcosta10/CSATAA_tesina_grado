# Clasificación de semillas aplicando ténicas de aprendizaje automático

**Tesina de grado - Licenciatura en Estadística**

**Universidad Nacional de Rosario - Facultad de Ciencias Económicas y Estadística**

*Alumno:* Victorio Costa

*Directora:* Dra. Daniela Dianda

*Co-directora:* Lic. Julia Fernandez

### Contenido

Este repositorio contiene dos carpetas, las cuales contienen todo el análisis estadístico realizado en las aplicaciones del trabajo.

La carpeta **Ap1_Pistachos** contiene los programas y bases utilziadas en la primera aplicación, titulada "Clasificación de frutos de pisatcho".

La carpeta **Ap2_Cuscuta** contiene los resultados de las evaluaciones realizadas y los programas con las métricas obtenidas para la segunda aplicación, titulada "Evaluación del desempeño del prototipo Seeds Analyzer".

#### Primera aplicación

* **Pistachio_Image_Dataset**: Carpeta con las 2148 imágenes originales de los frutos de pistacho.
* **10_data_split.R**: Script donde se definieron las unidades que serían enviadas al conjunto de entrenamiento, al de validación y al de testo.
* **11_id_pist.txt**: Archivo de texto generado por el script *0_data_split.R* con los id de las unidades pertenecientes a cada subconjunto.
* **12_data_split.ipynb**: Script que crea la carpeta *img* y separa las imágenes existentes en *Pistachio_Image_Dataset* en los subconjuntos de interés en base al archivo *id_pist.txt*.
* **13_tree.R**: Entrenamiento y testeo del modelo *AR*.
* **14_CNN_train.ipynb**: Entrenamiento de los modelos *R*2 Y *R*3.
* **15_100x100_2capas.keras**: Archivo que guarda los parámetros del modelo *R*2.
* **16_100x100_3capas.keras**: Archivo que guarda los parámetros del modelo *R*3.
* **17_CNN_test.ipynb**: Testeo de los modelos *R*2 Y *R*3.
* **18_metricas.R**: Sripct con cálculo y comparación de los tres modelos mediante medidas de desempeño predictivo.

Las imágenes de los frutos de pistacho se obtuvieron de:

OZKAN IA., KOKLU M. and SARACOGLU R. (2021). Classification of Pistachio Species Using Improved K-NN Classifier. Progress in Nutrition, Vol. 23, N. 2, pp. DOI:10.23751/pn.v23i2.9686.  (Open Access) https://www.mattioli1885journals.com/index.php/progressinnutrition/article/view/9686/9178

#### Segunda aplicación

* **pruebas.xlsx**: Archivo de Excel con los resultados de las 108 pruebas realizadas en el prototipo Seeds Analyzer.
* **20_aleatorizació.R**: Script que lleva a cabo la aleatorización del orden en el que se realizaron las pruebas del prototipo Seeds Analyzer.
* **21_cuscuta.R**: Script con cálculo y comparación de los dos métodos mediante medidas de desempeño predictivo.
