 Call:
 rpart(formula = Rating ~ ., data = db[, -1], control = list(minsplit = 5, 
     cp = 0.007, maxcompete = 0, maxsurrogate = 0))
   n= 1349 
 
             CP nsplit rel error    xerror       xstd
 1  0.260277900      0 1.0000000 1.0034111 0.04763004
 2  0.050568587      1 0.7397221 0.7457273 0.04742188
 3  0.043526889      2 0.6891535 0.6951306 0.04526162
4  0.016838450      3 0.6456266 0.6583608 0.04527700
 5  0.014096027      4 0.6287882 0.6654600 0.04597311
 6  0.011986207      5 0.6146921 0.6578210 0.04662123
7  0.011734739      6 0.6027059 0.6559195 0.04655569
 8  0.010451677      7 0.5909712 0.6468699 0.04641934
 9  0.009752959      8 0.5805195 0.6329144 0.04635076
 10 0.009677947     10 0.5610136 0.6240315 0.04594927
 11 0.007993203     12 0.5416577 0.5907967 0.03684478
## 12 0.007858888     13 0.5336645 0.5908004 0.03686014
## 13 0.007804769     15 0.5179467 0.5884963 0.03690525
## 14 0.007616245     16 0.5101420 0.5884963 0.03690525
## 15 0.007000000     17 0.5025257 0.5847114 0.03666189
## 
## Variable importance
##     TotalPasses   Interceptions           Goals   PositionShort 
##              62              13               8               6 
##      AerialLost    TotalCrosses  AccuratePasses AccurateCrosses 
##               5               2               2               2 
## 
## Node number 1: 1349 observations,    complexity param=0.2602779
##   mean=6.686086, MSE=0.1744474 
##   left son=2 (565 obs) right son=3 (784 obs)
##   Primary splits:
##       TotalPasses < 121.5 to the left,  improve=0.2602779, (0 missing)
## 
## Node number 2: 565 observations,    complexity param=0.05056859
##   mean=6.43508, MSE=0.1589599 
##   left son=4 (185 obs) right son=5 (380 obs)
##   Primary splits:
##       TotalPasses < 29.5  to the left,  improve=0.1325017, (0 missing)
## 
## Node number 3: 784 observations,    complexity param=0.04352689
##   mean=6.866977, MSE=0.1074823 
##   left son=6 (466 obs) right son=7 (318 obs)
##   Primary splits:
##       Interceptions < 13.5  to the left,  improve=0.1215573, (0 missing)
## 
## Node number 4: 185 observations,    complexity param=0.01683845
##   mean=6.227081, MSE=0.1256477 
##   left son=8 (171 obs) right son=9 (14 obs)
##   Primary splits:
##       Goals < 0.5   to the left,  improve=0.1704717, (0 missing)
## 
## Node number 5: 380 observations,    complexity param=0.01409603
##   mean=6.536342, MSE=0.1438611 
##   left son=10 (307 obs) right son=11 (73 obs)
##   Primary splits:
##       Interceptions < 4.5   to the left,  improve=0.06068009, (0 missing)
## 
## Node number 6: 466 observations,    complexity param=0.01173474
##   mean=6.772554, MSE=0.1061731 
##   left son=12 (416 obs) right son=13 (50 obs)
##   Primary splits:
##       Goals < 3.5   to the left,  improve=0.05581482, (0 missing)
## 
## Node number 7: 318 observations,    complexity param=0.007993203
##   mean=7.005346, MSE=0.07718966 
##   left son=14 (249 obs) right son=15 (69 obs)
##   Primary splits:
##       AccuratePasses < 971   to the left,  improve=0.07663216, (0 missing)
## 
## Node number 8: 171 observations,    complexity param=0.01198621
##   mean=6.185205, MSE=0.1012367 
##   left son=16 (154 obs) right son=17 (17 obs)
##   Primary splits:
##       PositionShort splits as  LLLLRL, improve=0.1629387, (0 missing)
## 
## Node number 9: 14 observations
##   mean=6.738571, MSE=0.1407694 
## 
## Node number 10: 307 observations,    complexity param=0.01045168
##   mean=6.490782, MSE=0.1232834 
##   left son=20 (214 obs) right son=21 (93 obs)
##   Primary splits:
##       PositionShort splits as  LRLLRL, improve=0.06498593, (0 missing)
## 
## Node number 11: 73 observations,    complexity param=0.009677947
##   mean=6.727945, MSE=0.1849588 
##   left son=22 (51 obs) right son=23 (22 obs)
##   Primary splits:
##       AerialLost < 1.5   to the right, improve=0.1590535, (0 missing)
## 
## Node number 12: 416 observations,    complexity param=0.007858888
##   mean=6.745865, MSE=0.09625983 
##   left son=24 (238 obs) right son=25 (178 obs)
##   Primary splits:
##       PositionShort splits as  LRLLRL, improve=0.04552944, (0 missing)
## 
## Node number 13: 50 observations,    complexity param=0.007804769
##   mean=6.9946, MSE=0.1334208 
##   left son=26 (42 obs) right son=27 (8 obs)
##   Primary splits:
##       AerialLost < 16    to the right, improve=0.2753232, (0 missing)
## 
## Node number 14: 249 observations
##   mean=6.964859, MSE=0.07678884 
## 
## Node number 15: 69 observations
##   mean=7.151449, MSE=0.05137471 
## 
## Node number 16: 154 observations,    complexity param=0.007616245
##   mean=6.142532, MSE=0.06987476 
##   left son=32 (145 obs) right son=33 (9 obs)
##   Primary splits:
##       Interceptions < 2.5   to the left,  improve=0.1665621, (0 missing)
## 
## Node number 17: 17 observations
##   mean=6.571765, MSE=0.2194145 
## 
## Node number 20: 214 observations,    complexity param=0.009752959
##   mean=6.431776, MSE=0.09834731 
##   left son=40 (146 obs) right son=41 (68 obs)
##   Primary splits:
##       Goals < 0.5   to the left,  improve=0.1042542, (0 missing)
## 
## Node number 21: 93 observations
##   mean=6.626559, MSE=0.1542161 
## 
## Node number 22: 51 observations
##   mean=6.615294, MSE=0.07032295 
## 
## Node number 23: 22 observations,    complexity param=0.009677947
##   mean=6.989091, MSE=0.3530901 
##   left son=46 (13 obs) right son=47 (9 obs)
##   Primary splits:
##       TotalCrosses < 4.5   to the right, improve=0.3099227, (0 missing)
## 
## Node number 24: 238 observations,    complexity param=0.007858888
##   mean=6.688613, MSE=0.08795984 
##   left son=48 (168 obs) right son=49 (70 obs)
##   Primary splits:
##       AccurateCrosses < 4.5   to the left,  improve=0.08959742, (0 missing)
## 
## Node number 25: 178 observations
##   mean=6.822416, MSE=0.09711495 
## 
## Node number 26: 42 observations
##   mean=6.910952, MSE=0.08935624 
## 
## Node number 27: 8 observations
##   mean=7.43375, MSE=0.1351734 
## 
## Node number 32: 145 observations
##   mean=6.115655, MSE=0.04979009 
## 
## Node number 33: 9 observations
##   mean=6.575556, MSE=0.1943136 
## 
## Node number 40: 146 observations
##   mean=6.362671, MSE=0.07785793 
## 
## Node number 41: 68 observations,    complexity param=0.009752959
##   mean=6.580147, MSE=0.110072 
##   left son=82 (61 obs) right son=83 (7 obs)
##   Primary splits:
##       AerialLost < 1.5   to the right, improve=0.3201313, (0 missing)
## 
## Node number 46: 13 observations
##   mean=6.713846, MSE=0.0397929 
## 
## Node number 47: 9 observations
##   mean=7.386667, MSE=0.5381333 
## 
## Node number 48: 168 observations
##   mean=6.63131, MSE=0.06146138 
## 
## Node number 49: 70 observations
##   mean=6.826143, MSE=0.1247608 
## 
## Node number 82: 61 observations
##   mean=6.516557, MSE=0.04570454 
## 
## Node number 83: 7 observations
##   mean=7.134286, MSE=0.3286816

Considere os dados de alguns jogadores disponíveis no data.frame abaixo.

##                    Name Height Weight Age AerialWon AerialLost TotalPasses
## 1                Helton    189     85  35         1          0         105
## 4       Marian Cisovsky    186     80  34         2          9          72
## 3      Sergio Rodríguez    169     65  21         7         12          28
## 9       Mushaga Bakenga    181     75  21         5         22          53
## 5           Ivan Martic    183     73  23         4          1          83
## 11 Geoffrey Mujangi Bia    176     71  24         1          0          74
##    AccuratePasses TotalCrosses AccurateCrosses Assists Goals Red Yellow
## 1              81            0               0       0     0   0      0
## 4              60            1               1       0     0   0      1
## 3              14            0               0       0     0   0      1
## 9              38            2               0       0     1   0      0
## 5              61           13               2       0     0   0      1
## 11             54           11               1       0     3   0      0
##    Interceptions PositionShort TotalShots ShotsOnTarget ShotsBlocked
## 1              0            GK          0             0            0
## 4              8             D          2             0            5
## 3              0             M          3             0            0
## 9              0            FW          7             3            0
## 5              7             M          2             0            3
## 11             2             M         14             8            0