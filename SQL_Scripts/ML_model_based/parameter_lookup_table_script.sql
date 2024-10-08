drop table rkc_male_sw_temp_params;

create table rkc_male_sw_temp_params 
(SURVEY_YEAR              NUMBER,
CRUISE                    NUMBER,
MALE_WEIGHTED_GEAR_TEMP   NUMBER,
a_base                    NUMBER,
aT                        NUMBER,
aT_OS                     NUMBER,
aOS                       NUMBER,
a_NS                      NUMBER,
a_OS                      NUMBER,
log10_A_NS                NUMBER,
log10_A_OS                NUMBER,
b                         NUMBER);

drop table smbkc_male_sw_temp_params;

create table smbkc_male_sw_temp_params 
(SURVEY_YEAR              NUMBER,
CRUISE                    NUMBER,
MALE_WEIGHTED_GEAR_TEMP   NUMBER,
a_base                    NUMBER,
aT                       NUMBER,
aT_OS                     NUMBER,
aOS                       NUMBER,
b                         NUMBER,
bt                        NUMBER,
bOS                       NUMBER,
bt_OS                     NUMBER,
a_NS                      NUMBER,
a_OS                      NUMBER,
log10_A_NS                NUMBER,
log10_A_OS                NUMBER,
B_NS                      NUMBER,
B_OS                      NUMBER);

drop table cb_male_sw_temp_params;

create table cb_male_sw_temp_params 
(SURVEY_YEAR              NUMBER,
CRUISE                    NUMBER,
MALE_WEIGHTED_GEAR_TEMP   NUMBER,
a_base                    NUMBER,
aT                       NUMBER,
aT_OS                     NUMBER,
aOS                       NUMBER,
b                         NUMBER,
bt                        NUMBER,
bOS                       NUMBER,
bt_OS                     NUMBER,
a_NS                      NUMBER,
a_OS                      NUMBER,
log10_A_NS                NUMBER,
log10_A_OS                NUMBER,
B_NS                      NUMBER,
B_OS                      NUMBER);
                    
drop table co_male_sw_temp_params;

create table co_male_sw_temp_params 
(SURVEY_YEAR              NUMBER,
CRUISE                    NUMBER,
MALE_WEIGHTED_GEAR_TEMP   NUMBER,
A_BASE                    NUMBER,
ATR                      NUMBER,  
AOS                       NUMBER,
B                           NUMBER,
BTR                          NUMBER,
BOS                      NUMBER,
A_NS                      NUMBER,
A_OS                      NUMBER,
LOG10_A_NS                NUMBER,
LOG10_A_OS                NUMBER,
B_NS                      NUMBER,
B_OS                   NUMBER);

------------Females ---------------

drop table rkc_female_sw_temp_params;

create table rkc_female_sw_temp_params 
(SURVEY_YEAR              NUMBER,
CRUISE                    NUMBER,
FEMALE_WEIGHTED_GEAR_TEMP   NUMBER,
a_base                    NUMBER,
aT                        NUMBER,
a                         NUMBER,
log10_A                NUMBER,
b                         NUMBER);


drop table cb_matfem_sw_temp_params;

create table cb_matfem_sw_temp_params 
(SURVEY_YEAR              NUMBER,
CRUISE                    NUMBER,
MATURE_FEMALE_WEIGHTED_GEAR_TEMP   NUMBER,
a_base                    NUMBER,
aT                       NUMBER,
aOS                       NUMBER,
b                         NUMBER,
bt                        NUMBER,
bOS                       NUMBER,
a_NS                      NUMBER,
a_OS                      NUMBER,
log10_A_NS                NUMBER,
log10_A_OS                NUMBER,
B_NS                      NUMBER,
B_OS                      NUMBER);
                    

drop table co_matfem_sw_temp_params;

create table co_matfem_sw_temp_params 
(SURVEY_YEAR              NUMBER,
CRUISE                    NUMBER,
MATURE_FEMALE_WEIGHTED_GEAR_TEMP   NUMBER,
a_base                    NUMBER,
aT                       NUMBER,
aOS                       NUMBER,
b                         NUMBER,
bt                        NUMBER,
bOS                       NUMBER,
a_NS                      NUMBER,
a_OS                      NUMBER,
log10_A_NS                NUMBER,
log10_A_OS                NUMBER,
B_NS                      NUMBER,
B_OS                      NUMBER);
