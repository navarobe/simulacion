###### LIBRERIAS  ######
########################
library(shiny)
library(ggplot2)

###### U I  ######
##################
ui <- navbarPage("Crédito por examen Alejandra Camacho Lastra 156216", id="principal",
                 
                 # Generadores aleatorios
                 tabPanel("Ej. 1",
                          tags$style("#switch_gen { display:none; }"),
                          tags$style("#switch_prueba { display:none; }"),
                          tags$style("#tipo_clases { display:none; }"),
                          sidebarLayout(
                              sidebarPanel(width = 4,
                                  fluidRow(selectInput("gen", "Generador", choices = c("Función de R"= "1", "Congruencial multiplicativo"= "2","Congruencial lineal"= "3"))),
                                  fluidRow(column(width = 6,numericInput("n_prueba", "n", value = 1000, min = 2, max = 1000000)),
                                           column(width = 6,numericInput("semilla", "Semilla", value = 84))),
                                  tabsetPanel(id= "switch_gen",
                                      tabPanel("1"),
                                      tabPanel("2",fluidRow(
                                               column(width = 7,numericInput("a_mult", "a", 1103515245, min = 1, max=2^32-1)),
                                               column(width = 5,textInput("m_mult", "m", value = "2^32")))),
                                      tabPanel("3",fluidRow(
                                               column(width = 7,numericInput("a_lin", "a", 1103515245, min = 1, max=2^32-1)),
                                               column(width = 5,textInput("m_lin", "m", value = "2^32"))),
                                               numericInput("c_lin", "c", 12345, min = 0, max = 2^32-1))
                                  ),
                                  fluidRow(column(width = 7,
                                                  selectInput("prueba", "Prueba",
                                              c("Ji-cuadrada"="1",
                                                "Prueba Serial"="2",
                                                "Kolmogorov-Smirnov"=3,
                                                "Cramer-von Mises"="4",
                                                "Prueba de las Corridas"="5",
                                                "Correlacion de Atrasos"="6"))
                                              ),
                                           column(width = 5,selectInput("alfa", "Alfa", c(.1, .05, .02, .01),selected = .05))),
                                  tabsetPanel(id="switch_prueba",
                                              tabPanel("0"),
                                              tabPanel("1",
                                                       numericInput("k_ji", "Numero de intervalos (k)", min=2, max=1000, value = 100, step = 1)),
                                              tabPanel("2",
                                                       numericInput("j_atraso", "Numero de atrasos (j)", min = 1, max= 100,  value = 1, step = 1 )))
                                  
                              ),
                              mainPanel(
                                  column(width = 2, downloadButton("descarga_prueba", label = NULL),tableOutput("datos_prueba")),
                                  column(width = 10,
                                         fluidRow(column(width = 6, tags$b("Hipotesis Nula:")), column(width = 6, textOutput("H0"))),
                                         fluidRow(column(width = 6, tags$b("Hipotesis Alternativa:")), column(width = 6, textOutput("HA"))),
                                         fluidRow(column(width = 6, tags$b("Valor del estadístico:")), column(width = 6, textOutput("est_prueba"))),
                                         fluidRow(column(width = 6, tags$b("La región de rechazo es:")), column(width = 6, textOutput("crit_prueba"))),
                                         fluidRow(column(width = 6, tags$b("El valor P es:")), column(width = 6, textOutput("vp_prueba"))),
                                         fluidRow(column(width = 6 ),column(width = 6,numericInput("clases_prueba", "Número de clases", min = 1, value = 100))),
                                         plotOutput("plot_prueba")),
                              )
                          )),
                 
                 # Distribuciones
                 tabPanel("Ej. 2",
                          tags$style("#par_dist { display:none; }"),
                          sidebarLayout(
                              sidebarPanel(
                                  numericInput("n_dist", "n", 1000),
                                  selectInput("tipo_dist", "Tipo de distribución", choices = c("Continua"="0", "Discreta"="1")),
                                  selectInput("dist_dist", "Distribución",NULL),
                                  tabsetPanel(id = "par_dist",
                                              tabPanel("1",fluidRow(column(width = 6,numericInput("a_u_c", "a", value = 0 )),column(width = 6, numericInput("b_u_c", "b", value = 1 )))),
                                              tabPanel("2",fluidRow(column(width = 6,numericInput("beta_exp", "beta", value = 1, min = 0.0)))),
                                              tabPanel("3",fluidRow(column(width = 6,numericInput("alfa_gamma","alfa", value = 1, min = 0)),column(width = 6, numericInput("beta_gamma","beta", value = 1, min = 0)))),
                                              tabPanel("4",fluidRow(column(width = 6,numericInput("alfa_wei","alfa", value = 1)),column(width = 6, numericInput("beta_wei","beta", value = 1)))),
                                              tabPanel("5",fluidRow(column(width = 6,numericInput("miu_normal", "miu", value = 0)),column(width = 6, numericInput("sigma_normal", "sigma", value = 1, min = 0)))),
                                              tabPanel("6",fluidRow(column(width = 6,numericInput("alfa_beta","alfa", value = 1, min = 0)),column(width = 6, numericInput("beta_beta","beta", value = 1, min = 0)))),
                                              tabPanel("7",fluidRow(column(width = 6,numericInput("miu_logno", "miu", value = 0)),column(width = 6, numericInput("sigma_logno", "sigma^2", value = 1, min = 0)))),
                                              tabPanel("8",fluidRow(column(width = 6,numericInput("v_ji", "grados de libertad", value = 10, min = 1, step = 1)))),
                                              tabPanel("9",fluidRow(column(width = 6,numericInput("v_t", "grados de libertad", value = 10, min = 1, step = 1)))),
                                              tabPanel("10",fluidRow(column(width = 6,numericInput("v1_f", "v1", value = 10, min = 1, step = 1)),column(width = 6, numericInput("v2_f", "v2", value = 10, min = 1, step = 1)))),
                                              tabPanel("11",fluidRow(column(width = 6,numericInput("a_unif", "a", value = 1)),column(width = 6, numericInput("b_unif", "b", value = 6)))),
                                              tabPanel("12",fluidRow(column(width = 6,numericInput("p_berno", "p", value = .5, min = 0, max = 1)))),
                                              tabPanel("13",fluidRow(column(width = 6,numericInput("nn_bino", "n", value = 10, min = 1, step = 1)),column(width = 6, numericInput("p_bino", "p", value = .5, min = 0, max = 1)))),
                                              tabPanel("14",fluidRow(column(width = 6,numericInput("p_geo", "p", value = .5, min = 0, max = 1)))),
                                              tabPanel("15",fluidRow(column(width = 6,numericInput("r_nega", "r", value = 2)),column(width = 6, numericInput("p_nega", "p", value = .5, min = 0, max = 1)))),
                                              tabPanel("16",fluidRow(column(width = 6,numericInput("lamd", "lambda", value = .5))))
                                              
                                  )
                              ),
                              mainPanel(
                                  column(width = 2, downloadButton("descarga_dist", label = NULL),tableOutput("datos_dist")),
                                  column(width = 10, tabsetPanel(id="tipo_clases",
                                      tabPanel("0",numericInput("clases_dist", "Número de clases", value = 100, min = 2)),
                                      tabPanel("1","")
                                  ),
                                         plotOutput("plot_dist"))
                              )
                          )
                 ),
                 
                 # Extras
                 tabPanel("Ej. 3",
                          tags$style("#switch_3 { display:none; }"),
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("aprox","Aproximación", choices = c("Estadísticas de orden"="1", "Chi^2 - Zn"="2")),
                                  tabsetPanel(id="switch_3",
                                              tabPanel("1", 
                                                    numericInput("n_a", "Número de muestras", min = 1, value = 1000),
                                                    numericInput("nn_a","Tamaño de muestra (n)",min=1, value = 10),
                                                    numericInput("teta_a", "Theta", min = 0, value = 1)
                                            ),
                                            tabPanel("2", 
                                                     numericInput("n_b", "Número de muestras", min = 1, value = 1000),
                                                     numericInput("grad_b","Grados de libertad (n)",min=1, value = 10)
                                            ) 
                                  )
                              ),
                              mainPanel(
                                  column(width = 2, downloadButton("descarga_3", label = NULL),tableOutput("datos_3")),
                                  column(width = 10,
                                         numericInput("clases_3", "Número de clases", value = 100, min = 2),
                                         plotOutput("plot_3"))
                              )
                          ))
    
)


##### FUNCIONES AUX ######

unif01_prueba<- function(n, a, m, semilla, c, gen){
    
    a<-as.numeric(a)
    m<-as.numeric(eval(parse(text= m)))
    semilla<- as.numeric(semilla)
    c<- as.numeric(c)
    if(gen==1){
        set.seed(semilla)
        ui<- runif(n)
    }else if(gen==2){
        zi<- c()
        ui<- c()
        zi[1]<- (a*semilla)%%m
        ui[1]<- zi[1]/m
        if(n>1){
            for(i in 2:n){
                zi[i]<- (a*zi[i-1])%%m
                ui[i]<- zi[i]/m}
        }
    }else if(gen==3){
        k0<- floor((a*semilla+c)/m)
        zi<- c()
        ki<- c()
        ui<- c()
        zi[1]<- (a*semilla+c)-m*k0
        ki[1]<- floor((a*zi[1]+c)/m)
        ui[1]<- zi[1]/m
        if(n>1){
            for(i in 2:n){
                zi[i]<- (a*zi[i-1]+c)-m*ki[i-1]
                ki[i]<- floor((a*zi[i]+c)/m)
                ui[i]<- zi[i]/m
            }}
    }
    return(ui)
    
}
prueba_chi<- function(ui, alfa, k, n){
    h0<- "Fx(x) = F0(x) para toda x  "
    ha<- "Fx(x) <> F0(x) para alguna x  "
    alfa<- as.numeric(alfa)
    Oi<- c()
    Oi<- table( cut(ui, breaks = k))
    Ei<- n/k
    Xi2<- sum((Oi-Ei)^2)/Ei
    PV_chi<-pchisq(Xi2, df= k-1, lower.tail = FALSE)
    crit_chi<-qchisq(alfa, df= k-1, lower.tail = FALSE)
    crit_chi<- paste("Todos los estadisticos X^2 >", crit_chi)
    res_prueba<- c(Xi2,PV_chi,crit_chi, h0, ha)
    return(res_prueba)
}
prueba_serial<- function(Ui, alfa, k, n, d){
    h0<-"La muestra proviende de una distribucion uniforme sobre el hipercubo [0,1]^2"
    ha<-"La muestra NO proviende de una distribucion uniforme sobre el hipercubo [0,1]^2"
    Ei<- (n/(k^d))
    alfa<- as.numeric(alfa)
    Ui<- matrix(Ui, ncol = d, byrow = TRUE)
    ffj<-NULL
    if(d==1){
        ffj<-  table(cut(Ui[,1], breaks = k))
    }else if(d==2){
        ffj<-  table(cut(Ui[,1], breaks = k),cut(Ui[,2], breaks = k))
    }else if(d==3){
        ffj<-  table(cut(Ui[,1], breaks = k),cut(Ui[,2], breaks = k),cut(Ui[,3], breaks = k))
    }else if(d==4){
        ffj<-  table(cut(Ui[,1], breaks = k),cut(Ui[,2], breaks = k),cut(Ui[,3], breaks = k),cut(Ui[,4], breaks = k))
    }else if(d==5){
        ffj<-  table(cut(Ui[,1], breaks = k),cut(Ui[,2], breaks = k),cut(Ui[,3], breaks = k),cut(Ui[,4], breaks = k),cut(Ui[,5], breaks = k))
    }
    Xi2_serial<-sum((ffj-Ei)^2)/Ei
    PV_serial<-pchisq(Xi2_serial, df= (k^d)-1, lower.tail = FALSE)
    crit_serial<-qchisq(alfa, df= (k^d)-1, lower.tail = FALSE)
    crit_serial<- paste("Todos los estadisticos X^2 >", crit_serial)
    serial<- c(Xi2_serial,PV_serial, crit_serial, h0, ha )
    return(serial)
    
}
prueba_ks<- function(ui, alfa, n){
    h0<- "Fx(x) = F0(x) para toda x  "
    ha<- "Fx(x) <> F0(x) para alguna x  "
    alfa<- as.numeric(alfa)
    Fu<-sort(ui)
    Fn<-c()
    dif<-c()
    difabs<-c()
    for (i in 1:n) {
        Fn[i]<-i/n
        dif[i]<-Fu[i]-Fn[i]
        difabs[i]<-abs(dif[i])
    }
    Dn<-max(difabs)
    valores_crit<- c(1.07, 1.22, 1.36, 1.52, 1.63)/sqrt(n)
    alfas<-c(.2, .1, .05, .02, .01)
    names(valores_crit)<- alfas
    crit_s_k<- valores_crit[which(alfas==alfa)]
    crit_s_k<- paste("Todos los estadisticos Dn >", crit_s_k)
    PV_SK<- ""
    if(Dn<valores_crit[1]){
        PV_SK<- "es mayor a 0.2"
    }else if(Dn>valores_crit[5]){
        PV_SK<- "es menor a 0.01"
    }else{
        i1<- which(valores_crit<=Dn)[length(which(valores_crit<=Dn))]
        i2<- which(valores_crit>=Dn)[1]
        PV_SK<- paste("esta entre ", alfas[i1], "y ", alfas[i2])
    }
    sk<- c(Dn, PV_SK, crit_s_k,h0,ha)
    return(sk)
}
prueba_cvm<- function(ui, alfa, n){
    h0<- "Fx(x) = F0(x) para toda x  "
    ha<- "Fx(x) <> F0(x) para alguna x  "
    alfa<- as.numeric(alfa)
    N<-c(2,3,4,5,6,7,8,9,10,20,50,200,1000, Inf)
    co1<-c(0.55052,0.63976,0.67017,0.68352,0.69443,0.70154,0.70912,0.71283,0.71582,0.72948,0.73784,0.74205,0.74318,0.74346) #1-alpha=0.99
    co2<-c(0.48897,0.53316,0.54200,0.55056,0.55572,0.55935,0.56327,0.56513,0.56663,0.57352,0.57775,0.57990,0.58047,0.58061) #0.975
    co3<-c(0.42482,0.43938,0.44199,0.44697,0.44911,0.45100,0.45285,0.45377,0.45450,0.45788,0.45995,0.46101,0.46129,0.46136) #0.95
    co4<-c(0.34346,0.33786,0.34183,0.34238,0.34352,0.34397,0.34462,0.34491,0.34514,0.34621,0.34686,0.34719,0.34728,0.34730) #0.90
    co5<-c(0.28851,0.27961,0.28337,0.28305,0.28331,0.28345,0.28358,0.28361,0.28368,0.28387,0.28398,0.28404,0.28406,0.28406) #0.85
    co6<-c(0.24743,0.24169,0.24260,0.24236,0.24198,0.24197,0.24187,0.24180,0.24175,0.24150,0.24134,0.24126,0.24124,0.24124) #0.80
    co7<-c(0.21521,0.21339,0.21173,0.21165,0.21110,0.21087,0.21066,0.21052,0.21041,0.20990,0.20960,0.20944,0.20940,0.20939) #0.75
    co8<-c(0.12659,0.12542,0.12405,0.12252,0.12200,0.12158,0.12113,0.12088,0.12069,0.11979,0.11924,0.11897,0.11890,0.11888) #0.50
    co9<-c(0.09145,0.07683,0.07494,0.07427,0.07352,0.07297,0.07254,0.07228,0.07208,0.07117,0.07052,0.07035,0.07027,0.07206) #0.25
    co10<-c(0.07351,0.06986,0.06581,0.06511,0.06548,0.06492,0.06448,0.06423,0.06403,0.06312,0.06258,0.06231,0.06224,0.06222) #0.20
    co11<-c(0.06554,0.06092,0.05895,0.05799,0.05747,0.05697,0.05650,0.05625,0.05605,0.05515,0.05462,0.05435,0.05428,0.05426) #0.15
    co12<-c(0.05785,0.05287,0.05093,0.04970,0.04910,0.04869,0.04823,0.04798,0.04778,0.04689,0.04635,0.04610,0.04603,0.04601) #0.10
    tabla<-matrix(c(co1,co2,co3,co4,co5,co6,co7,co8,co9,co10,co11,co12), nrow = 14)
    alfas<-c(0.01, 0.025, 0.05, 0.1, 0.15, 0.2, 0.25, 0.5, 0.75, 0.8, 0.85, 0.9)
    colnames(tabla)<- alfas
    numorde<-sort(ui)
    sumanum=0
    for(i in 1:n){
        sumanum=sumanum+((numorde[i]-(2*i-1)/(2*n))^2)
    }
    y=sumanum+(1/(12*n))
    
    crit_cvm<- NULL
    alfa_crit<-which(alfas == alfa)
    a_crit<- which(N<=n)[length(which(N<=n))]
    b_crit<- which(N>=n)[1]
    if(a_crit== b_crit){
        crit_cvm<-paste("es", tabla[a_crit, alfa_crit])
    }else{
        crit_cvm<- paste("esta entre", tabla[a_crit, alfa_crit], "y", tabla[b_crit, alfa_crit])
    }
    
    PV_cvm<- NULL
    a_pv<-which(tabla[a_crit,]<= y)[1]
    b_pv<-which(tabla[a_crit,]>= y)[length(which(tabla[a_crit,]>= y))]
    c_pv<-which(tabla[b_crit,]<= y)[1]
    d_pv<-which(tabla[b_crit,]>= y)[length(which(tabla[b_crit,]>= y))]
    pv_max<-max(a_pv,b_pv,c_pv,d_pv)
    pv_min<-min(a_pv,b_pv,c_pv,d_pv)
    
    if(pv_max== pv_min){
        if(pv_max== 1){
            PV_cvm<-paste("es menor a", alfas[pv_min])
        }else if(pv_max== 12){
            PV_cvm<-paste("es mayor a", alfas[pv_min])
        }else{
            PV_cvm<-paste("es ", alfas[pv_min])
        }
        
    }else{
        PV_cvm<- paste("esta entre", alfas[pv_min], "y", alfas[pv_max])
    }
    rech<-tabla[b_crit, alfa_crit]
    rech<- paste("Todos los estadisticos Y >", rech) 
    cvm<- c(y, PV_cvm,rech , h0,ha)
    return(cvm)
    
}
prueba_corrida<- function(ui, alfa, n){
    h0<-"Las Ui's son variables aleatorias independientes"
    ha<-"Las Ui's NO son variables aleatorias independientes"
    a_ij<- matrix(c(4529.4, 9044.9, 13568, 18091, 22615, 27892,
                    9044.9, 18097, 27139, 36187, 45234, 55789,
                    13568, 27139, 40721, 54281, 67852, 83685,
                    18091, 36187, 54281, 72414, 90470, 111580,
                    22615, 45234, 67852, 90470, 113262, 139476,
                    27892, 55789, 83685, 111580, 139476, 172860), nrow = 6)
    bi<- c(1/6, 5/24, 11/120, 19/720, 29/5040, 1/840)
    alfa<- as.numeric(alfa)
    corridas<- c()
    contador<- 1
    corr<- 1
    ui0<- c(ui,0)
    for(i in 1:(n)){
        if(ui0[i]<=ui0[i+1]){
            corr<- corr+1
        }else{
            corridas[contador]<- corr
            contador<-contador+ 1
            corr<- 1
        }
    }
    ri<- c()
    for(i in 1:5){ri[i]<-length(which(corridas==i))}
    ri[6]<- length(which(corridas>=6))
    R<- sum((ri-n*bi)%*%t((ri-n*bi))*a_ij)/n
    PV_corr<-pchisq(R, df= 6, lower.tail = FALSE)
    crit_corr<-qchisq(alfa, df= 6, lower.tail = FALSE)
    crit_corr<- paste("Todos los estadisticos R >",crit_corr)
    corrida<- c(R, PV_corr, crit_corr, h0,ha)
    return(corrida)
}
prueba_atraso<- function(ui, alfa, n, j){
    h0<- "ro_j = 0 "
    ha<- "ro_j <> 0"
    alfa<- as.numeric(alfa)
    h<- floor((n-1)/j)-1
    v_pj<- (13*h+7)/(h+1)^2
    ui1<- ui[(1+0*j) :(1+h*j)]
    ui2<- ui[(1+(0+1)*j) : (1+(h+1)*j) ]
    ro_j<- (12/(h+1))*sum(ui1*ui2)-3
    A_j<- ro_j/sqrt(v_pj)
    vp_atraso<- 2*pnorm(A_j, lower.tail = FALSE)
    crit_atraso<- qnorm(alfa/2, lower.tail = FALSE)
    crit_atraso<- paste("Todos los estadisticos |Aj| > ", crit_atraso)
    atraso<- c(A_j,vp_atraso,crit_atraso,h0,ha)
    return(atraso)
}

unif01<- function(n,gene){
    a<-as.numeric(gene[1])
    m<-as.numeric(eval(parse(text= gene[2])))
    semilla<- as.numeric(abs(sample(.Random.seed, 1)))
    c<- as.numeric(gene[3])
    gen<- as.numeric(gene[4])
    if(gen==1){
        ui<- runif(n)
    }else if(gen==2){
        zi<- c()
        ui<- c()
        zi[1]<- (a*semilla)%%m
        ui[1]<- zi[1]/m
        if(n>1){
            for(i in 2:n){
                zi[i]<- (a*zi[i-1])%%m
                ui[i]<- zi[i]/m}
        }
    }else if(gen==3){
        k0<- floor((a*semilla+c)/m)
        zi<- c()
        ki<- c()
        ui<- c()
        zi[1]<- (a*semilla+c)-m*k0
        ki[1]<- floor((a*zi[1]+c)/m)
        ui[1]<- zi[1]/m
        if(n>1){
            for(i in 2:n){
                zi[i]<- (a*zi[i-1]+c)-m*ki[i-1]
                ki[i]<- floor((a*zi[i]+c)/m)
                ui[i]<- zi[i]/m
            }}
    }
    return(ui)
    
}
uniforme_ab<- function(n,gene,a,b){
    x<- a+(b-a)*unif01(n,gene)
    x<- as.data.frame(x)
    return(x)
}
exponencial_beta<- function(n,gene,beta){
    x<- -beta*log(unif01(n,gene))
    x<- as.data.frame(x)
    return(x)
}
gamma_01<- function(n,gene,alfa, beta){
    b<- (exp(1)+alfa)/exp(1)
    x<- c()
    
    for(i in 1:n){
        repeat{
            u1<- unif01(1,gene)
            u2<- unif01(1,gene)
            
            if(u1>(1/b)){
                y<- -log((b*(1-u1))/alfa)
                if(u2<=y^(alfa-1)){
                    x[i]<- y*beta
                    break}
            }else{
                y<-(b*u1)^(1/alfa)
                if(u2<=exp(-y)){
                    x[i]<- y*beta
                    break
                }
            }
        }
    }
    x<- as.data.frame(x)
    return(x)
}
gamma_0mas<- function(n,gene, alfa_, beta_){
    if(alfa_<=1){
        x<- gamma_01(n,gene, alfa_, beta_)
    }else{
        a<- 1/sqrt(2*alfa_-1)
        b<- alfa_- log(4)
        q<- alfa_ + 1/a
        teta<- 4.5
        d<- 1+ log(teta)
        conta<- 1
        x<- c()
        for(i in 1:n){
            repeat{
                u1<- unif01(1,gene)
                u2<- unif01(1,gene)
                v<- a*log(u1/(1-u1))
                y<- alfa_*exp(v)
                z<- u1^2 * u2
                w<- b+q*v-y
                
                if((w+d-teta*z>=0) | (w >= log(z))){
                    x[i]<- y*beta_
                    break
                }
            }
        }
        
    }
    x<- as.data.frame(x)
    return(x)
}
weibull<- function(n,gene, alfa, beta){
    x<- beta*(-log(1-unif01(n,gene)))^(1/alfa)
    x<- as.data.frame(x)
    return(x)
}
normal_<- function(n,gene, miu, sigma){
    u1<- unif01(n,gene)
    u2<- unif01(n,gene)
    x1<- sqrt(-2*log(u1))*cos(2*pi*u2)
    x2<- sqrt(-2*log(u1))*sin(2*pi*u2)
    x1<- miu + sigma*x1
    x2<- miu + sigma*x2
    x<- sample(c(x1, x2), n)
    x<- as.data.frame(x)
    return(x)
}
beta_gen<- function(n,gene, alfa, beta){
    x<- c()
    for(i in 1:n){
        repeat{
            u1<- unif01(1,gene)
            u2<- unif01(1,gene)
            y1<- u1^(1/alfa)
            y2<- u2^(1/beta)
            if((y1+y2)<=1){
                x[i]<- y1/(y1+y2)
                break
            }
        }
    }
    x<- as.data.frame(x)
    return(x)
}
lognormal<- function(n,gene, miu, sigsq){
    sig<- sqrt(sigsq)
    z<- normal_(n,gene, 0, 1)
    x<- exp(miu + sig*z)
    x<- as.data.frame(x)
    return(x)
}
ji_cua<- function(n,gene, v){
    
    x<- c()
    for(i in 1:n){
        if(v==1){
            z<- normal_(1,gene, 0, 1)
        }else{
            z<- normal_(v,gene, 0, 1)}
        y<- z^2
        x[i]<- sum(y)
    }
    x<- as.data.frame(x)
    return(x)
}
tstudent<- function(n,gene, v){
    z<- normal_(n,gene, 0,1)
    w<- ji_cua(n,gene, v)
    x<- z/sqrt(w/v)
    x<- as.data.frame(x)
    return(x)
}
ffisher<- function(n_,gene, v1, v2){
    w1<- ji_cua(n_,gene, v1)
    w2<- ji_cua(n_,gene, v2)
    x<- (w1/v1)/(w2/v2)
    x<- as.data.frame(x)
    return(x)
}
unif_dis<- function(n,gene,a, b){
    reco<- c(a:b)
    ff<- rep(1/(length(reco)), (length(reco)))
    u<- unif01(n,gene)
    x<- c()
    for(i in 1:n) {
        j<- 0
        Aj<- 0
        Aj_<- 0
        repeat{
            j<- j+1
            Aj<- Aj_+ff[j]
            Aj_<- Aj
            if(u[i]<Aj){
                x[i]<- reco[j]
                break}
        }
    }
    x<- as.data.frame(x)
    return(x)
}
berno<- function(n,gene, p){
    x<- c()
    u<- unif01(n,gene)
    for(i in 1:n){
        if(u[i]<=p){
            x[i]<- 1
        }else{
            x[i]<- 0
        }
    }
    x<- as.data.frame(x)
    return(x)
}
bino<- function(n,gene, nn, p){
    x<- c()
    for(i in 1:n){
        y<- berno(nn,gene, p)
        x[i]<- sum(y)
    }
    x<- as.data.frame(x)
    return(x)
}
geometrica<- function(n,gene, p){
    u<- unif01(n,gene)
    x<- floor(log(u)/log(1-p))
    x<- as.data.frame(x)
    return(x)
}
binega<- function(n,gene, r, p){
    x<- c()
    for(i in 1:n){
        y<- geometrica(r,gene, p)
        x[i]<- sum(y)
    }
    x<- as.data.frame(x)
    return(x)
}
pois<- function(n,gene, lam){
    x<- c()
    for(i in 1:n) {A<- 1
    k<- 0
    repeat{u<- unif01(1,gene)
    A<- A*u
    if(A<exp(-lam)){
        x[i]<- k
        break
    }else{
        k<- k+1
    }
    }}
    x<- as.data.frame(x)
    return(x)
}

a_3<- function(n, gene, m, teta){
    m<- as.numeric(m)
    teta<- as.numeric(teta)
    Zn<-c()
    for(i in 1:n){
        yn<- max(uniforme_ab(m,gene, 0,teta))
        Zn[i]<- m*(teta-yn)
    }
    Zn<- as.data.frame(Zn)
    return(Zn)
}
b_3<- function(n,gene, m){
    yn<- ji_cua(n,gene,m)
    Zn<- (yn-m)/sqrt(2*m)
    Zn<- as.data.frame(Zn)
    return(Zn)
}

###### SERVER  ######
#####################
server <- function(input, output,session) {
    
    colors<- c("Función Empírica"="dodgerblue2", "Función Teórica"="gold")
    colors_a<- c("Densidad Zn"="dodgerblue2", "Distribucion Exponencial"="gold")
    colors_b<- c("Densidad Zn"="dodgerblue2", "Distribucion Normal(0,1)"="gold")
    
    observeEvent(input$principal,{
        if(input$principal=="Ej. 1"){
        observeEvent(input$gen,{ 
        updateTabsetPanel(session, "switch_gen", selected = input$gen)
        updateNumericInput(session, "a_mult", value = input$a_lin)
        updateNumericInput(session, "a_lin", value = input$a_mult)
        updateTextInput(session, "m_mult", value = input$m_lin)
        updateTextInput(session, "m_lin", value = input$m_mult)
    })
    observeEvent(input$prueba,{
        updateTabsetPanel(session, "switch_prueba", {
            if(input$prueba %in% c(1,2)){
            selected = "1"
        }else if(input$prueba == 6){
            selected = "2"
        }else{
            selected = "0"
        }})
        
    })
    UI<- reactive(unif01_prueba((input$n_prueba*2), input$a_lin, input$m_lin, input$semilla, input$c_lin, input$gen))
    ui<- reactive(unif01_prueba(input$n_prueba, input$a_lin, input$m_lin, input$semilla, input$c_lin, input$gen))
    output$datos_prueba<- renderTable({ui()},colnames = TRUE)
    output$plot_prueba<- renderPlot({
        ggplot(data=as.data.frame(ui()), aes(x=ui()))+
            geom_histogram(aes(fill="Función Empírica",y=..density..), color="dodgerblue2" ,alpha=.5,breaks= seq(0,1, 1/input$clases_prueba))+
            stat_function(fun=dunif, alpha= .5, geom = "area", aes(fill= "Función Teórica"))+
            labs(x="x",y="Densidad", title = "X ~ Uniforme (0,1)", fill="")+
            scale_fill_manual(values = colors)+
            theme(legend.position = "bottom")
    })
    observeEvent(input$prueba,{
        if(input$prueba==1){
            respu_prueba<-reactive(prueba_chi(ui(), input$alfa, input$k_ji, input$n_prueba))
            output$est_prueba<- renderText({respu_prueba()[1]})
            output$vp_prueba<- renderText({respu_prueba()[2]})
            output$crit_prueba<- renderText({respu_prueba()[3]})
            output$H0<- renderText({respu_prueba()[4]})
            output$HA<- renderText({respu_prueba()[5]})
        }else if(input$prueba==2){
            respu_prueba<-reactive(prueba_serial(UI(), input$alfa, input$k_ji, input$n_prueba,2))
            output$est_prueba<- renderText({respu_prueba()[1]})
            output$vp_prueba<- renderText({respu_prueba()[2]})
            output$crit_prueba<- renderText({respu_prueba()[3]})
            output$H0<- renderText({respu_prueba()[4]})
            output$HA<- renderText({respu_prueba()[5]})
        }else if(input$prueba==3){
            respu_prueba<-reactive(prueba_ks(ui(), input$alfa, input$n_prueba))
            output$est_prueba<- renderText({respu_prueba()[1]})
            output$vp_prueba<- renderText({respu_prueba()[2]})
            output$crit_prueba<- renderText({respu_prueba()[3]})
            output$H0<- renderText({respu_prueba()[4]})
            output$HA<- renderText({respu_prueba()[5]})
        }else if(input$prueba==4){
            respu_prueba<-reactive(prueba_cvm(ui(), input$alfa, input$n_prueba))
            output$est_prueba<- renderText({respu_prueba()[1]})
            output$vp_prueba<- renderText({respu_prueba()[2]})
            output$crit_prueba<- renderText({respu_prueba()[3]})
            output$H0<- renderText({respu_prueba()[4]})
            output$HA<- renderText({respu_prueba()[5]})
        }else if(input$prueba==5){
            respu_prueba<-reactive(prueba_corrida(ui(), input$alfa, input$n_prueba))
            output$est_prueba<- renderText({respu_prueba()[1]})
            output$vp_prueba<- renderText({respu_prueba()[2]})
            output$crit_prueba<- renderText({respu_prueba()[3]})
            output$H0<- renderText({respu_prueba()[4]})
            output$HA<- renderText({respu_prueba()[5]})
        }else if(input$prueba==6){
            respu_prueba<-reactive(prueba_atraso(ui(), input$alfa, input$n_prueba, input$j_atraso))
            output$est_prueba<- renderText({respu_prueba()[1]})
            output$vp_prueba<- renderText({respu_prueba()[2]})
            output$crit_prueba<- renderText({respu_prueba()[3]})
            output$H0<- renderText({respu_prueba()[4]})
            output$HA<- renderText({respu_prueba()[5]})
        }
    })
    output$descarga_prueba<- downloadHandler(
        filename = function() {
            paste(input$n_prueba,"-numeros-pseudoaleatorios", ".csv", sep="")
        },
        content = function(file){
            write.csv(ui(),file)
        }
    )}
        
        else if(input$principal=="Ej. 2"){
            gene<- reactive({c(input$a_lin, input$m_lin, input$c_lin, input$gen)})
            distribuciones<- c("Uniforme continua"= "1",
                               "Exponencial"="2",
                               "Gamma"="3",
                               "Weibull"="4",
                               "Normal"="5",
                               "Beta"="6",
                               "Lognormal"="7",
                               "Ji-Cuadrada"="8",
                               "t-Student"="9",
                               "F-Fisher"= "10",
                               "Uniforme discreta"="11",
                               "Bernoulli"="12",
                               "Binomial"="13",
                               "Geométrica"="14",
                               "Binomial negativa"="15",
                               "Poisson"="16")
            observeEvent(input$tipo_dist, {
                if(input$tipo_dist==0){
                    updateSelectInput(session, "dist_dist", choices = distribuciones[1:10])
                }else{
                    updateSelectInput(session, "dist_dist", choices = distribuciones[11:16])
                }
                updateTabsetPanel(session,"tipo_clases", selected = input$tipo_dist)
            })
            observeEvent(input$dist_dist,{
                updateTabsetPanel(session, "par_dist", selected = input$dist_dist)
                if(input$dist_dist=="1"){
                    x<-reactive({uniforme_ab(input$n_dist,gene(), input$a_u_c, input$b_u_c)})
                    output$plot_dist<- renderPlot({
                        ggplot(data=x(), aes(x=x))+
                            geom_histogram(aes(y=..density.., fill="Función Empírica"),breaks = seq(input$a_u_c, input$b_u_c, (input$b_u_c-input$a_u_c)/input$clases_dist), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dunif, args = list(min=input$a_u_c, max=input$b_u_c), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ Uniforme (", input$a_u_c,",", input$b_u_c,")"), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="2"){
                    x<-reactive({exponencial_beta(input$n_dist,gene(),input$beta_exp)})
                    output$plot_dist<- renderPlot({
                        ggplot(data=x(), aes(x=x))+
                            geom_histogram(aes(y=..density.., fill="Función Empírica"),breaks = seq(0, max(x()), (max(x()))/input$clases_dist), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dexp, args = list(rate= 1/input$beta_exp), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ Exponencial (", input$beta_exp,")"), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="3"){
                    x<-reactive({gamma_0mas(input$n_dist, gene(),  input$alfa_gamma, input$beta_gamma)})
                    output$plot_dist<- renderPlot({
                        ggplot(data=x(), aes(x=x))+
                            geom_histogram(aes(y=..density.., fill="Función Empírica"),breaks = seq(0, max(x()), (max(x()))/input$clases_dist), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dgamma, args = list(shape=input$alfa_gamma, scale= input$beta_gamma), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ Gamma (",input$alfa_gamma, ",", input$beta_gamma,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="4"){
                    x<-reactive({weibull(input$n_dist, gene(), input$alfa_wei, input$beta_wei)})
                    output$plot_dist<- renderPlot({
                        ggplot(data=x(), aes(x=x))+
                            geom_histogram(aes(y=..density.., fill="Función Empírica"),breaks = seq(0, max(x()), (max(x()))/input$clases_dist), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dweibull, args = list(shape=input$alfa_wei, scale= input$beta_wei), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ Weibull (",input$alfa_wei, ",", input$beta_wei,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="5"){
                    x<-reactive({normal_(input$n_dist, gene(), input$miu_normal, input$sigma_normal)})
                    output$plot_dist<- renderPlot({
                        ggplot(data = x(), aes(x=x))+
                            geom_histogram(aes(fill="Función Empírica" ,y=..density..),bins = input$clases_dist, alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dnorm, args = list(mean=input$miu_normal, sd= input$sigma_normal), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ Normal (",input$miu_normal, ",", input$sigma_normal,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="6"){
                    x<-reactive({beta_gen(input$n_dist, gene(), input$alfa_beta, input$beta_beta)})
                    output$plot_dist<- renderPlot({
                        ggplot(data = x(), aes(x=x))+
                            geom_histogram(aes(fill="Función Empírica" ,y=..density..),breaks = seq(0, 1, 1/input$clases_dist), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dbeta, args = list(shape1=input$alfa_beta, shape2= input$beta_beta), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~  Beta (",input$alfa_beta, ",", input$beta_beta,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="7"){
                    x<-reactive({lognormal(input$n_dist,gene(), input$miu_logno, input$sigma_logno)})
                    output$plot_dist<- renderPlot({
                        ggplot(data = x(), aes(x=x))+
                            geom_histogram(aes(fill="Función Empírica" ,y=..density..),breaks = seq(0, max(x()), (max(x()))/input$clases_dist), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dlnorm, args = list(meanlog=input$miu_logno, sdlog= sqrt(input$sigma_normal)), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ Lognormal (",input$miu_logno, ",", input$sigma_logno,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="8"){
                    x<-reactive({ji_cua(input$n_dist, gene(), input$v_ji)})
                    output$plot_dist<- renderPlot({
                        ggplot(data = x(), aes(x=x))+
                            geom_histogram(aes(fill="Función Empírica" ,y=..density..),breaks = seq(0, max(x()), (max(x()))/input$clases_dist), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dchisq, args = list(df=input$v_ji), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ Chi^2 (",input$v_ji,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="9"){
                    x<-reactive({tstudent(input$n_dist,gene(), input$v_t)})
                    output$plot_dist<- renderPlot({
                        ggplot(data = x(), aes(x=x))+
                            geom_histogram(aes(fill="Función Empírica" ,y=..density..),bins = input$clases_dist, alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dt, args = list(df=input$v_t), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ t-Student (",input$v_t,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="10"){
                    x<-reactive({ffisher(input$n_dist,gene(), input$v1_f, input$v2_f)})
                    output$plot_dist<- renderPlot({
                        ggplot(data = x(), aes(x=x))+
                            geom_histogram(aes(fill="Función Empírica" ,y=..density..),breaks = seq(0, max(x()), (max(x()))/input$clases_dist), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = df, args = list(df1=input$v1_f, df2=input$v2_f), alpha= .5, geom = "area", aes(fill= "Función Teórica"), color="gold")+
                            labs(x="x", y="Densidad", title = paste("X ~ F-Fisher (",input$v1_f, ",", input$v2_f,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="11"){
                    x<-reactive({unif_dis(input$n_dist,gene(), input$a_unif, input$b_unif)})
                    dreal<- reactive({
                        x<- c(input$a_unif:input$b_unif)
                        y<- rep(1/length(x), length(x))
                        data.frame(x,y)
                    })
                    output$plot_dist<- renderPlot({
                        ggplot(data=NULL, aes())+
                            geom_bar(data=x(), aes(x=x,y=..prop.. , fill= "Función Empírica"), alpha=.5, color="dodgerblue2",width = 1)+
                            geom_bar(data=dreal(), aes(x=x, y=y , fill="Función Teórica"), color="gold", stat = "identity", alpha= .5, width = 1)+
                            labs(x="x", y="Densidad", title = paste("X ~ Uniforme [",input$a_unif, ",", input$b_unif,"]" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="12"){
                    x<-reactive({berno(input$n_dist,gene(), input$p_berno)})
                    dreal<- reactive({
                        x<- c(0,1)
                        y<- dbinom(x, 1, input$p_berno)
                        data.frame(x,y)
                    })
                    output$plot_dist<- renderPlot({
                        ggplot(data=NULL, aes())+
                            geom_bar(data=x(), aes(x=x,y=..prop.. , fill= "Función Empírica"), alpha=.5, color="dodgerblue2",width = 1)+
                            geom_bar(data=dreal(), aes(x=x, y=y , fill="Función Teórica"), color="gold", stat = "identity", alpha= .5, width = 1)+
                            labs(x="x", y="Densidad", title = paste("X ~ Bernoulli (",input$p_berno,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="13"){
                    x<-reactive({bino(input$n_dist, gene(),input$nn_bino, input$p_bino)})
                    dreal<- reactive({
                        x<- c(0:input$nn_bino)
                        y<- dbinom(x, input$nn_bino, input$p_bino)
                        data.frame(x,y)
                    })
                    output$plot_dist<- renderPlot({
                        ggplot(data=NULL, aes())+
                            geom_bar(data=x(), aes(x=x,y=..prop.. , fill= "Función Empírica"), alpha=.5, color="dodgerblue2",width = 1)+
                            geom_bar(data=dreal(), aes(x=x, y=y , fill="Función Teórica"), color="gold", stat = "identity", alpha= .5, width = 1)+
                            labs(x="x", y="Densidad", title = paste("X ~ Binomial (",input$nn_bino, ",", input$p_bino,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="14"){
                    x<-reactive({geometrica(input$n_dist, gene(), input$p_geo)})
                    dreal<- reactive({
                        x<- c(0:max(x()))
                        y<- dgeom(x, input$p_geo)
                        data.frame(x,y)
                    })
                    output$plot_dist<- renderPlot({
                        ggplot(data=NULL, aes())+
                            geom_bar(data=x(), aes(x=x,y=..prop.. , fill= "Función Empírica"), alpha=.5, color="dodgerblue2",width = 1)+
                            geom_bar(data=dreal(), aes(x=x, y=y , fill="Función Teórica"), color="gold", stat = "identity", alpha= .5, width = 1)+
                            labs(x="x", y="Densidad", title = paste("X ~ Geométrica (",input$p_geo,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="15"){
                    x<-reactive({binega(input$n_dist,gene(), input$r_nega, input$p_nega)})
                    dreal<- reactive({
                        x<- c(0:max(x()))
                        y<- dnbinom(x,input$r_nega, input$p_nega)
                        data.frame(x,y)
                    })
                    output$plot_dist<- renderPlot({
                        ggplot(data=NULL, aes())+
                            geom_bar(data=x(), aes(x=x,y=..prop.. , fill= "Función Empírica"), alpha=.5, color="dodgerblue2",width = 1)+
                            geom_bar(data=dreal(), aes(x=x, y=y , fill="Función Teórica"), color="gold", stat = "identity", alpha= .5, width = 1)+
                            labs(x="x", y="Densidad", title = paste("X ~ Binomial Negativa (",input$r_nega, ",", input$p_nega,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }else if(input$dist_dist=="16"){
                    x<-reactive({pois(input$n_dist,gene(), input$lamd)})
                    dreal<- reactive({
                        x<- c(0:max(x()))
                        y<- dpois(x, input$lamd)
                        data.frame(x,y)
                    })
                    output$plot_dist<- renderPlot({
                        ggplot(data=NULL, aes())+
                            geom_bar(data=x(), aes(x=x,y=..prop.. , fill= "Función Empírica"), alpha=.5, color="dodgerblue2",width = 1)+
                            geom_bar(data=dreal(), aes(x=x, y=y , fill="Función Teórica"), color="gold", stat = "identity", alpha= .5, width = 1)+
                            labs(x="x", y="Densidad", title = paste("X ~ Poisson (",input$lamd,")" ), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors)
                    })
                }
                
                  
                output$datos_dist<-renderTable({x()})
                output$descarga_dist<- downloadHandler(
                    filename = function() {
                        paste(input$n_dist,"-números-de-distribución-",names(distribuciones[as.numeric(input$dist_dist)]), ".csv", sep="")
                    },
                    content = function(file){
                        write.csv(x(),file)
                    }
                )
            })
            
            
            
        }
        
        else if(input$principal=="Ej. 3"){
            gene<- reactive({c(input$a_lin, input$m_lin, input$c_lin, input$gen)})
            observeEvent(input$aprox,{
                updateTabsetPanel(session, "switch_3", selected = input$aprox )
                if(input$aprox==1){
                    x<- reactive({a_3(input$n_a,gene(),input$nn_a,input$teta_a)})
                    output$plot_3<- renderPlot({
                        ggplot(data=x(), aes(x=Zn))+
                            geom_histogram(aes(y=..density.., fill="Densidad Zn"),breaks = seq(0, max(x()), (max(x()))/input$clases_3), alpha=.5, color="dodgerblue2")+
                            stat_function(fun = dexp, args = list(rate= 1/input$teta_a), alpha= .5, geom = "area", aes(fill= "Distribucion Exponencial"), color="gold")+
                            labs(x="Zn", y="Densidad", title = paste("Zn ~ n*(𝜃- Yn) ; Yn ~ max(U(0,𝜃)) ; 𝜃=", input$teta_a,")"), fill="")+
                            theme(legend.position = "bottom")+
                            scale_fill_manual(values = colors_a)
                    })
                }else if(input$aprox==2){
                    x<- reactive({b_3(input$n_b,gene(),input$grad_b)})
                    output$plot_3<-renderPlot({
                        ggplot(data=x(), aes(x=x))+
                        geom_histogram(aes(fill="Densidad Zn" ,y=..density..),bins = input$clases_3, alpha=.5, color="dodgerblue2")+
                        stat_function(fun = dnorm, args = list(mean=0, sd= 1), alpha= .5, geom = "area", aes(fill= "Distribucion Normal(0,1)"), color="gold")+
                        labs(x="Zn", y="Densidad", title = "", fill="")+
                        theme(legend.position = "bottom")+
                        scale_fill_manual(values = colors_b)})
                }
                output$datos_3<-renderTable({x()})
                output$descarga_3<- downloadHandler(
                    filename = function() {
                        paste("Zn", ".csv", sep="")
                    },
                    content = function(file){
                        write.csv(x(),file)
                    }
                )
            })
            
        }
        })
    
    
    
}

###################################
shinyApp(ui = ui, server = server)
