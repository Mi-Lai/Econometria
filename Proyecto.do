use "H:\7MO\ECONOMETRIA\codigos_base de datos\ANTEPROYECTO.dta", clear
/*Prueba de raiz unitaria - Prueba Dickey-Fuller*//
twoway connected pe ao
twoway connected sin_oc ao
twoway connected sin_pag ao
twoway connected ilpa ao
twoway connected def ao
twoway connected ev ao
twoway connected itlp ao
twoway connected pib ao
twoway connected pob_ocupada ao
twoway connected t_ni5 ao
twoway connected ump ao
twoway connected pfalf_1524 ao
twoway connected minpo_15_29 ao
twoway connected pea_15 ao
twoway connected poet ao
twoway connected tp_15 ao
twoway connected ism ao
dfuller pe, drift regress lags(3)
dfuller sin_oc, drift regress lags(3)
dfuller sin_pag, drift regress lags(3)
dfuller ilpa, drift regress lags(3)
dfuller def, drift regress lags(3)
dfuller ev, drift regress lags(3)
dfuller itlp, drift regress lags(3)
dfuller pib, drift regress lags(3)
dfuller pob_ocupada, drift regress lags(3)
dfuller t_ni5, drift regress lags(3)
dfuller ump, drift regress lags(3)
dfuller pfalf_1524, drift regress lags(3)
dfuller minpo_15_29, drift regress lags(3)
dfuller  pea_15, drift regress lags(3)
dfuller  poet, drift regress lags(3)
dfuller  tp_15, drift regress lags(3)
dfuller  ism, drift regress lags(3)
/*Correlación*/
twoway (scatter  lpe  lump) (lfit  lpe lump)
twoway (scatter  lpe  lpfalf_1524 ) (lfit  lpe  lpfalf_1524)
twoway (scatter  lpe  lminpo_15_29 ) (lfit  lpe  lminpo_15_29)
twoway (scatter  lpe   lpea_15) (lfit  lpe  lpea_15)
twoway (scatter  lpe   lpoet) (lfit  lpe lpoet)

/*Estimamos la regresión*/
 reg  lpe  lump lpfalf_1524 lminpo_15_29 lpea_15 lpoet

/*Prueba de normalidad*/
/*residuales estandarizados*/

predict error,r
predict error_st,rsta
tsline error, name(error)
tsline error_st, name(error_st)
twoway (scatter  error error_st) (lfit error error_st)
hist error,normal
qnorm error
twoway (scatter  error ao) (lfit error ao)
sktest error
swilk  error
sfrancia  error
sum error, d
*residuals versus fitted values
rvfplot 
*(residual versus predictor plot
rvpplot lpe
rvpplot 
*lista de estimaciones 
ereturn list
predict e,r
twoway (scatter e lpe)
/* Prueba Jarque -Bera*/
gen e2=error^2
gen e3=error^3
gen e4=error^4
scalar list
scalar m4=r(mean)
sum e2,meanonly
scalar m2=r(mean)
sum e3, meanonly
scalar m3=r(mean)
sum e4,meanonly
scalar m4=r(mean)
scalar list
scalar q1= m3/m2^(2/3)
scalar q2= m4/m2^(2)

*scalar jb=(_N)*((g1)^2/6+(g2-3)^2/24
scalar jb=(_N)*(0.8364844^2/6+((2.884218-3)^2)/24)
display jb
display "valor critico= " invchi2(2,0.95)
display "p-value= " chi2tail(2,jb)

/*Autocorrelación */
reg  pe  ump pfalf_1524 minpo_15_29 pea_15 poet
predict error,r
predict error_st,rsta
tsline error, name(e)
twoway (scatter  e e_st) (lfit e e_st)
twoway (scatter  error L.error)

tsline  lpe lump lpfalf_1524 lminpo_15_29 lpea_15 lpoet
/*Correlación para cada variable en función de la v. dependiente*/

reg  lpe  lump lpfalf_1524 lminpo_15_29 lpea_15 lpoet
estat dwatson
estat bgodfrey, lags(1)

twoway (scatter  pe  sin_oc) (lfit  pe sin_oc)
twoway (scatter  lpe  lsin_oc) (lfit  lpe lsin_oc)
twoway (scatter  pe  sin_oc) (lfit  pe sin_oc)
twoway (scatter  pe  sin_oc) (lfit  pe sin_oc)
twoway (scatter  pe  sin_oc) (lfit  pe sin_oc)

predict e1,r
predict e1_st,rsta
tsline e1, name(e1)
tsline e_st, name(e_st)
twoway (scatter  e1 e1_st) (lfit e1 e1_st)
twoway (scatter  e1 e1_st)
twoway (scatter  e1 L.e1)


/* prueba de raices unitarias
H0: variable es estacionaria */
reg
predict e,r
tsline e
twoway(scatter e time,c (1) title("Errores de la regresión"))
hist e,den
qnorm e
sktest e
swilk  e
sfrancia  e
sum e, d

imtest,white
estat hettest
estat archlm
ac e
corrgram e
predict hatDI
tsline hatDI
### test Breusch-Pagan / Cook-Weisberg####
##  Prueba la varianza es función lineal del ajuste. ## 
reg  lpe  lump lpfalf_1524 lminpo_15_29 lpea_15 lpoet
predict ls_est
predict e,r
*/ suma de cuadrados de los errores */
display e(rss) 			
gen e2_bp= e^2 / (e(rss)/e(N))               
reg e2_bp ls_est
*/ suma de cuadrados de la regresión */
display e(mss)			 
display "Estadístico de prueba = " e(mss) / 2   
scalar est_prb=e(mss) / 2             
display "p-value= " chi2tail(1,e(mss) / 2)
display "valor critico= " invchi2(1,0.05)
quietly reg lpe  lump lpfalf_1524 lminpo_15_29 lpea_15 lpoet
hettest
/*Criterio de Akaike*/
 dis  .294929585/15
dis log(0.01966197)
dis 2*(-log(0.01966197)+5)
/*Prueba de Ramsey*/
reg  lpe  lump lpfalf_1524 lminpo_15_29 lpea_15 lpoet
predict y
g y2=y^2
g y3=y^3
g y4=y^4
reg  y2 y3 y4
test  y2= y3= y4=0
ovtest

/*Prubas de hipotesis*/
display ( 5.41235894/5) /(.294929585 /9)
display Ftail(5,9,33.032448)
*Significancia individual de la regresión
* lump: Unidades médicas privadas
display "t-value" = ( 6.615647/ 2.752422)
*Significancia individual de la regresión
* lump: Unidades médicas privadas
display "t-value     " = ( 6.615647/ 2.752422)
* pfalf_1524 Pob.femenina alfabeta
display "t-value     " = (  90.45656/11.62881)
* lminpo_15_29  Mediana de ingreso
display "t-value     " = (   -5.500498 /  1.990338 )
*lpea_15 Pob. Economicamente activa
display "t-value     " = (-15.49826/ 4.261584  )
* lpoet Pob. en edad de trabajar
display "t-value     " = ( 41.9383 / 5.506481  )
 *t tabla
display "t(15, 0.975)=   " invttail(15,0.025)


