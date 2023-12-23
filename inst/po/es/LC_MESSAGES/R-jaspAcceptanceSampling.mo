��    N      �  k   �      �  $   �  E   �       $   4  
   Y  _   d  !   �  $   �               /     H     _  @   x  W   �  '   	     9	     J	     `	     y	     �	  C   �	  1   �	  1   
  1   P
     �
     �
     �
  J   �
  <   	  S   F  �   �  �   f     �       G   /  b   w  e   �  i   @     �  Y   �     $  4   ;  $   p  	   �     �  y   �  z   3     �     �     �     �  ?   �  c   ?  E   �  V   �     @     S     Z     f     r     �     �  7   �  ~   �  3   g     �  n   �          ?  C   E  s   �  {   �  g   y     �  a   �     I  �  O  "   �  Q   
  !   \  (   ~     �  w   �  ,   1  (   ^     �     �     �     �     �  R     o   f  9   �          #     >     V     r  \   �  F   �  E   .  E   t     �     �     �  ^   	  S   h  ]   �  �     �        �     �  [   �  t   B   y   �   }   1!  -   �!  o   �!     M"  F   h"  &   �"  
   �"     �"     �"     }#     �#     $     '$     :$  N   R$  i   �$  K   %  k   W%     �%     �%     �%     �%     &      #&     D&  L   Z&  �   �&  :   8'     s'  �   �'  .   (     G(  M   M(  �   �(  �   ))     �)     D*  r   J*     �*        4   /                       7       J   <         ;   N   A   #   M   8   	       I   >      -       
             2   +           B   "          C          !         5       $       1   )       K   E       F   ?                          *   :   &   (   9   D       3                  .                        %   @          '   H   L            =                    6       ,   G      0       (Incoming) Proportion Non-conforming 1 - α (Producer's risk) has to be greater than β (consumer's risk). <u>Decision:</u> <b>%s</b> lot. AOQ (Average Outgoing Quality) Curve AOQL: %.3f AQL (Acceptable Quality Level) value should be lower than RQL (Rejectable Quality Level) value. ASN (Average Sample Number) Curve ATI (Average Total Inspection) Curve Acc. Number Accept or Reject Lot %s Acceptance Probabilities Acceptance Probability Acceptance Sampling Plan Acceptance number (c) cannot be larger than the sample size (n). Acceptance numbers (c) are cumulative, so they need to be in a non-decreasing sequence. Acceptance probabilities at AQL and RQL Actual P(accept) Analyze Variable Plan Average Outgoing Quality Average Sample Number Average Total Inspection Can not accept or reject lot: sample size has to be greater than 1. Can not calculate AOQ. Check the plan parameters. Can not calculate ASN. Check the plan parameters. Can not calculate ATI. Check the plan parameters. Create Variable Plan Critical Distance (k) Cum. Sample Size Cumulative sample size (n1+n2+...) cannot be larger than the lot size (N). Current plan <b>CAN %s</b> meet the specified risk point(s). Final rejection number (r) needs to be 1 more than the final acceptance number (c). For all stages except the last stage, rejection numbers (r) have to be at at least 2 greater than the acceptance numbers (c).

                                   Else, subsequent stages become redundant. For hypergeometric distribution, N * proportion non-conforming should all be integer values.

Check the values of N and proportion non-conforming. Generated Sampling Plan Historical Standard Deviation If historical standard deviation is unknown, sample size has to be > 1. If the number of defective items out of %1$d sampled is <= %2$d, accept the lot. Reject otherwise. Lot size (N = %.0f) cannot be smaller than the sample size (n = %.0f) of the generated variable plan. Lot size (N = %1$.0f) cannot be smaller than the sample size (n = %2$.0f) of the generated variable plan. Lower Specification Limit (LSL) Lower limit for proportion non-conforming items needs to be smaller than the upper limit. Multiple Sampling Plan No valid values found in the plan. Check the inputs. OC (Operating Characteristics) Curve P(accept) Probability of Acceptance Probability of acceptance (%.3f) at AQL (%.3f) is <b>lower</b> than the required probability of acceptance (%.3f) at AQL. Probability of acceptance (%.3f) at RQL (%.3f) is <b>higher</b> than the required probability of acceptance (%.3f) at RQL. Prop. Non-conforming Proportion Non-conforming Rej. Number Rejection Probability Rejection number (r) cannot be larger than the sample size (n). Rejection number (r) for every stage has to be larger than the corresponding acceptance number (c). Rejection number (r) has to be larger than the acceptance number (c). Rejection numbers (r) are cumulative, so they need to be in a non-decreasing sequence. Required P(accept) Sample Sample Mean Sample Size Sample Standard Deviation Sample mean is invalid. Sample size Sample size (n) cannot be larger than the lot size (N). Sample size has to be <b>> 1</b> if <b>both</b> LSL and USL are provided, and historical standard deviation is <b>unknown</b>. Sample standard deviation has to be greater than 0. Single Sampling Plan Step size of 0 is allowed only if the lower and upper limits of proportion non-conforming items are identical. Upper Specification Limit (USL) Value Variable Sampling Plan (Standard deviation assumed to be <b>%s</b>) Variable plan generated for the current quality constraints has an invalid k value. Modify the quality constraints. Variable plan generated for the current quality constraints has an invalid sample size (n). Modify the quality constraints. Variable plan generated for the current quality constraints is invalid. Modify the quality constraints. Z.LSL Z.LSL = (mean - LSL) / historical standard deviation

Accept lot if Z.LSL >= k, otherwise reject. Z.USL Project-Id-Version: jaspAcceptanceSampling 0.16.4
PO-Revision-Date: 2023-10-13 19:33+0000
Last-Translator: ecadrian <ecadrian@hotmail.com>
Language-Team: Spanish <https://hosted.weblate.org/projects/jasp/jaspacceptancesampling-r/es/>
Language: es
MIME-Version: 1.0
Content-Type: text/plain; charset=UTF-8
Content-Transfer-Encoding: 8bit
Plural-Forms: nplurals=2; plural=n != 1;
X-Generator: Weblate 5.1-dev
 (Entrante) Proporción No Conforme 1 - α (riesgo del productor) tiene que ser mayor que β (riesgo del consumidor). <u>Decisión:</u> <b>%s</b> lote. Curva CMS (Calidad Media de Salida, AOQ) LCMS (AOQL): %.3f El valor NCA (Nivel de Calidad Aceptable, AQL) tiene que ser menor que el valor NCR (Nivel de Calidad Rechazable, RQL). Curva NMM (Número de la Muestra Media, ASN) Curva ITM (Inspección Total Media, ATI) Número de Aceptación Aceptar o Rechazar Lote %s Probabilidades de Aceptación Probabilidad de Aceptación Plan de Muestreo de Aceptación El número de aceptación (c) no puede ser mayor que el tamaño de la muestra (n). Los números de aceptación (c) son acumulados, por lo que es necesario que no sigan una secuencia decreciente. Probabilidades de aceptación a los NCA (AQL) y NCR (RQL) P actual (aceptar) Analizar Plan de Variables Calidad Media de Salida Número de la Muestra Media Inspección Total Media No se puede aceptar ni rechazar el lote: el tamaño de la muestra tiene que ser mayor que 1. No se puede calcular la CMS (AOQ). Comprueba los parámetros del plan. No se puede calcular el NMM (ASN). Comprueba los parámetos del plan. No se puede calcular la ITM (ATI). Comprueba los parámetos del plan. Crear Plan de Variables Distancia Crítica (k) Tamaño de la Muestra Acumulada El tamaño de la muestra acumulada (n1+n2+...) no puede ser mayor que el tamaño del lote (N). El plan vigente <b>CAN %s</b> satisface el(los) punto(s) especificado(s) de riesgo. El número de rechazo final (r) tiene que ser 1 más que el número de aceptación final (c). Para todas las etapas excepto la última, los números de rechazo (r) deben ser al menos 2 veces mayores que los números de aceptación (c).

                                   De lo contrario, las etapas posteriores se vuelven redundantes. Para la distribución hipergeométrica, N * la proporción no conforme deben ser todos valores enteros.

Verifica los valores de N y la proporción no conforme. Plan de Muestreo Generado Desviación Típica Histórica Si la desviación típica histórica es desconocida, el tamaño de la muestra debe ser > 1. Si el número de elementos defectuosos de %1$d muestreados es <= %2$d, acepta el lote. Recházalo en caso contrario. El tamaño del lote (N = %.0f) no puede ser menor que el tamaño de la muestra (n = %.0f) del plan de variables generado. El tamaño del lote (N = %1$.0f) no puede ser menor que el tamaño de la muestra (n = %2$.0f) del plan de variables generado. Límite Inferior de Especificación (LIE,LSL) El límite inferior para la proporción de elementos no conformes, tiene que ser menor que el límite superior. Plan de Muestreo Múltiple No se encontraron valores válidos en el plan. Comprueba las entradas. Curva OC (Características Operativas) P(aceptar) Probabilidad de Aceptación La probabilidad de aceptación (%.3f) al NCA (%.3f) es <b>menor</b> que la probabilidad de aceptación requerida (%.3f) al NCA. La probabilidad de aceptación (%.3f) al NCR (%.3f) es <b>mayor</b> que la probabilidad de aceptación requerida (%.3f) al NCR. Prop. no conforme Proporción No Conforme Número de Rechazo Probabilidad de Rechazo El número de rechazo (r) no puede ser mayor que el tamaño de la muestra (n). El número de rechazo (r) de cada etapa debe ser mayor que el correspondiente número de aceptación (c). El número de rechazo (r) debe ser mayor que el número de aceptación (c). Los números de rechazo (r) son acumulados, por lo que es necesario que no sigan una secuencia decreciente. P requerido (aceptar) Muestra Media Muestral Tamaño de la muestra Desviación Típica Muestral La media muestral no es válida. Tamaño de la muestra El tamaño de la muestra (n) no puede ser mayor que el tamaño del lote (N). El tamaño muestral tiene que ser <b>> 1</b> si se indican <b>tanto</b> LIE como LSE, y la desviación típica histórica es <b>desconocida</b>. La desviación típica muestral tiene que ser mayor que 0. Plan de Muestreo Único Un tamaño de paso igual a 0 solo se admite si los límites inferior y superior de la proporción de elementos no conformes, son idénticos. Límite Superior de Especificación (LSE, USL) Valor Plan de Muestro de Variables (asumiendo una desviación típica de <b>%s</b>) El plan de variables generado para las restricciones de calidad vigentes, no tiene un valor k válido. Modifica las restricciones de calidad. El plan de variables generado para las restricciones de calidad vigentes, no tiene un tamaño muestral (n) válido. Modifica las restricciones de calidad. El plan de variables generado para las restricciones de calidad vigentes, no es válido. Modifica las restricciones de calidad. Z.LIE Z.LSL = (media - LSL) / desviación estándar histórica

Acepta el lote si Z.LSL >= k; de lo contrario, rechazar. Z.LSE 