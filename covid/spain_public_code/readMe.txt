01_modelling_madrid.R
-Runs BYM models using mobility data, as well as model validation (3 models total).
-outputs madrid_1spatial... and madrid_2spatial_....

02_modelling_castilla.R
-same as madrid
-outputs castilla_1spatial... and castilla_samples_identity

03_graphs.R
-Used for making figures in the manuscript

BYM2_2spatialeffects.stan
This is the main model used, inspired by Morris et al.

BYM2_slater.stan
This is for validation - modified from Morris et al.