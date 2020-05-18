fm1Orth.gls <- gls( distance ~ Sex * I( age - 11), Orthodont)
fm1Orth_fix.gls <- gls( distance ~ Sex * I( age - 11), Orthodont,
control = glsControl(sigma = 2))
fm1Dial.gnls <- gnls( rate ~ SSasympOff( pressure, Asym, lrc, c0), Dialyzer,
params = list( Asym + lrc ~ QB, c0 ~ 1),
start = c(53.6, 8.6, 0.51, -0.26, 0.225),
control = gnlsControl(sigma = NULL))
fm1Orth.lme <- lme( distance ~ I(age-11), Orthodont)
fm1Orth_fix.lme <- lme( distance ~ I(age-11), Orthodont,
control = lmeControl(sigma = 1))
fm3Orth.lme <- lme( distance ~ Sex * I( age - 11), Orthodont,
weights = varIdent(form = ~ 1 | Sex))
fm3Orth_fix.lme <- lme( distance ~ Sex * I( age - 11), Orthodont,
weights = varIdent(form = ~ 1 | Sex),
control = list(sigma = 2))
fm1Wafer.lme <- lme( current ~ voltage + I( voltage^2 ), Wafer,
random = list( Wafer = pdDiag( ~ voltage + I( voltage^2 )),
Site = pdDiag( ~ voltage + I( voltage^2 )) ),
control = lmeControl( sigma = NULL))
fm1Theo.nlme<-nlme(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
Theoph, fixed = lKe + lKa + lCl ~ 1,
start=c( -2.4, 0.45, -3.2),
control = nlmeControl( sigma = NULL))
fm2Theo.nlme<-nlme(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
Theoph, fixed = lKe + lKa + lCl ~ 1,
start=c( -2.4, 0.45, -3.2),
control = nlmeControl( sigma = NULL),
random = pdDiag( lKe + lKa + lCl ~ 1))
fm2Theo.nlme<-nlme(conc ~ SSfol(Dose, Time, lKe, lKa, lCl),
Theoph, fixed = lKe + lKa + lCl ~ 1,
start=c( -2.4, 0.45, -3.2),
control = nlmeControl( sigma = NULL),
random = pdDiag( lKa + lCl ~ 1))
∑
∑
∑
∑
∑
∑
∑
∑
∑
∑
∑
∗
∗
|
|
∗
∗
∗
∗


q
∆
∆
