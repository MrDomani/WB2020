############        F.Ieva, A.M.Paganoi, J.Romo, N.Tarabelloni       ############
############ roahd Package: Robust Analysis of High Dimensional Data ############

library(roahd)
######### Examples of univariate functional data 
######### Example 1 
P = 1e2
grid <- seq(0, 1, length.out = P)
values <- matrix(c( sin(2 * pi * grid),
                    cos(2 * pi * grid),
                    4 * grid * (1 - grid),
                    tan(grid),
                    log(grid)),
                 nrow = 5, ncol = 100, byrow = TRUE)
fD <- fData(grid, values)
plot(fD, main = 'Univariate FD', xlab = 'time [s]', ylab = 'values', lwd = 2)

######### Example 2 
N=50
P = 1e3
grid = seq( 0, 1, length.out = P ) 
C = exp_cov_function( grid, alpha = 0.2, beta = 0.3 )
values = generate_gauss_fdata( N,
                               centerline = sin( 2 * pi * grid ),
                               Cov = C )
fD = fData( grid, values )

plot(fD, main = 'Gaussian fData', xlab ='grid')

######### Examples of multivariate functional data
N = 100
Data = generate_gauss_mfdata( N, 2,
                       centerline = matrix( c( sin( 2 * pi * seq( 0, 1, length.out = 10^3 )),
                                               cos( 2 * pi * seq( 0, 1, length.out = 10^3 ) ) ), 
                                            nrow = 2, byrow = TRUE ),
                       correlations = 0.5,
                       listCov = list(exp_cov_function( seq( 0, 1, length.out = 10^3 ),
                                                        alpha = 0.1, beta  = 0.5 ),
                                      exp_cov_function( seq( 0, 1, length.out = 10^3 ),
                                                        alpha = 0.5, beta  = 0.1 )))

mfD = mfData( grid, Data )
plot( mfD, main = list( 'Comp.1', 'Comp. 2'), xlab = 'grid')

######### computation of depth measures
######### univariate functional data
depths = MBD(fD)
depths

######### multivariate functional data
depths = multiMBD(mfD, weights="uniform")
depths

######## median of a dataset
median = median_mfData(mfD_healthy, type = "multiMBD")

######## Spearman correlation coefficient for bivariate functional data
cor_spearman(mfD, ordering ="MEI")

######## Kendall index for bivariate functional data
cor_kendall(mfD, ordering="area")

######## computation of a BCA confidence interval for the Spearman correlation coefficient
BCIntervalSpearman(mfD$fDList[[1]], mfD$fDList[[2]], ordering = 'MEI',  
                   alpha=0.05, bootstrap_iterations = 1000)


######## computation of a BCA confidence interval for the Spearman correlation coefficient 
######## multivariate version
mfD_healthy_subset = as.mfData(list(mfD_healthy$fDList[[1]],
                                    mfD_healthy$fDList[[2]]))

cor_spearman(mfD_healthy_subset, ordering='MEI')

BCIntervalSpearmanMultivariate(mfD_healthy_subset,
                               ordering='MEI', alpha=0.05, bootstrap_iterations = 1000)


####### comparison test between two correlation matrices of multivariate functional data
mfD_healthy_subset = as.mfData(list(mfD_healthy$fDList[[1]],
                                    mfD_healthy$fDList[[2]]))

mfD_LBBB_subset = as.mfData(list(mfD_LBBB$fDList[[1]],
                                 mfD_LBBB$fDList[[2]]))

BTestSpearman(mfD_healthy_subset, mfD_LBBB_subset,
              bootstrap_iterations = 1000,
              ordering = "MEI", normtype = "f")

####### construction of the functional boxplot 

fbplot(mfD_healthy$fDList[[1]], Depths="MBD", Fvalue=3,
       main="Functional Boxplot")

####### construction of the multivariate functional boxplot 
mfD_healthy_subset <- as.mfData(list(mfD_healthy$fDList[[1]],
                                     mfD_healthy$fDList[[2]]))

fbplot( mfD_healthy_subset, Fvalue = 1.5, xlab = 'time',
        ylab = list( 'Values 1', 'Values 2' ),
        main = list( 'First component', 'Second component' ) )
