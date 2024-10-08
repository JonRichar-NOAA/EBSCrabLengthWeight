-- This script produces a table of population estimates for bairdi Tanner
-- crab from the 1975-2016 EBS trawl surveys.  Population is calculated
-- for males and female crab by stock assessment size category or maturity for each district. 
-- Abundance is calculated for unsexed crab, but biomass is not as we have no
-- size-weight regression factors for unsexed crab.

-- This script requires as input the master crab table (ebscrab) populated
-- with the survey data to be analyzed; a subset of the racebase.haul
-- table containing the haul data for the cruises being analyzed; 
-- and a strata lookup table.
-------------------------------------------------------------------------------

-- Don't want to average non-rkc in BB retow years, get rid of haul type 17 tows

drop table haul_newtimeseries_noretow;

create table haul_newtimeseries_noretow as
select * from haul_newtimeseries
where haul_type <> 17;


-- Create tables of raw catch by 1-mm size bin and sex
-- Separate by sex because male size group categories require shell condition
-- and female weights (post-2009) require clutch size

drop table cb_number_size1_male;

create table cb_number_size1_male as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,
shell_condition,(trunc(width/1) * 1)size1,
(sum(CASE
		 when species_code = 68560
		 and sex = 1
		 then sampling_factor
		 else 0
		 end)) number_male_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 68560
and width <> 999
and c.hauljoin(+) = h.hauljoin
and haul_type <> 17
group by c.hauljoin,
	  	 c.vessel,
		 c.cruise,
		 c.haul,
		 h.gis_station,
		 species_code,
     shell_condition,
		 (trunc(width/1) * 1);


-- Females (done separately from males because need clutch size info)

drop table cb_number_size1_female;

create table cb_number_size1_female as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,shell_condition,clutch_size,
(trunc(width/1) * 1)size1,
(sum(CASE
		 when species_code = 68560
		 and sex = 2
		 then sampling_factor
		 else 0
		 end)) number_female_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 68560
and width <> 999
and c.hauljoin(+) = h.hauljoin
and haul_type <> 17
group by c.hauljoin,
	  	 c.vessel,
		 c.cruise,
		 c.haul,
		 h.gis_station,
		 species_code,
        shell_condition,
     clutch_size,
		 (trunc(width/1) * 1);


-- unsexed

drop table cb_number_size1_unsexed;

create table cb_number_size1_unsexed as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,
(trunc(width/1) * 1)size1,
(sum(CASE
		 when species_code = 68560
		 and sex = 3
		 then sampling_factor
		 else 0
		 end)) number_unsexed_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 68560
and width <> 999
and c.hauljoin(+) = h.hauljoin
and haul_type <> 17
group by c.hauljoin,
	  	 c.vessel,
		 c.cruise,
		 c.haul,
		 h.gis_station,
		 species_code,
		 (trunc(width/1) * 1);



--  This section calculates the weight of the bairdi Tanner crab by haul, sex,
--  shell condition and 1-mm size group.  A width-weight regression
--  factor is applied, and multiplied by the number of crab caught in that
--  haul/sex/shellcon/size bin (from above section).  
--  The regression factor does not include unsexed crab, therefore no weights
--  will be calculated for unsexed crab


drop table cb_weight_grams_male;

create table cb_weight_grams_male as
select hauljoin,vessel,cruise,haul,gis_station,species_code,shell_condition,size1,
(CASE
--    WHEN cruise < 201001
--      THEN ((0.00019 * (power(size1,3.09894))) * number_male_size1)
    WHEN cruise >= 197501 and shell_condition in (1,2)
      THEN ((0.00027 * (power(size1,3.01425))) * number_male_size1)
    WHEN cruise >= 197501 and shell_condition in (0,3,4,5)
      THEN ((0.00021 * (power(size1,3.09197))) * number_male_size1)
    ELSE 0
    END) wgt_male_size1
from cb_number_size1_male 
order by cruise,vessel,haul,gis_station,size1;

drop table cb_weight_grams_female;

create table cb_weight_grams_female as
select hauljoin,vessel,cruise,haul,gis_station,species_code,shell_condition,clutch_size,size1,
(CASE
--    WHEN cruise < 201001
--      THEN ((0.00182 * (power(size1,2.70462))) * number_female_size1)
    WHEN cruise >= 197501 and clutch_size <= 1
      THEN ((0.000490 * (power(size1,2.854783))) * number_female_size1)
    WHEN cruise >= 197501 and shell_condition in (1,2) and clutch_size > 1
      THEN ((0.000457 * (power(size1,2.88374))) * number_female_size1)
    WHEN cruise >= 197501 and shell_condition in (0,3,4,5) and clutch_size > 1
      THEN ((0.000632 * (power(size1,2.824072))) * number_female_size1)
    ELSE 0
    END) wgt_female_size1
from cb_number_size1_female
order by cruise,vessel,haul,gis_station,species_code,shell_condition,clutch_size,size1;

-- Using actual female maturity in this run, so select for clutch size here

drop table cb_number_size1_matfem;

create table cb_number_size1_matfem as
select hauljoin, vessel, cruise, haul, gis_station, species_code, clutch_size,size1,
	   (sum(CASE
	   			WHEN   clutch_size = 0  
				   THEN number_female_size1
				ELSE 0
				END))  number_female_size1_immature,	
	   (sum(CASE
	   			WHEN   clutch_size >= 1  
				   THEN number_female_size1
				ELSE 0
				END))  number_female_size1_mature
    from cb_number_size1_female
    where species_code = 68560
		 --and width <> 999
	     --and hauljoin(+) = h.hauljoin
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code,
                clutch_size,
				size1;
        
-- And calculate weight of crab by actual maturity        

drop table cb_weight_grams_matfem;

create table cb_weight_grams_matfem as
select hauljoin, vessel, cruise, haul, gis_station, species_code, size1,
	   (sum(CASE
	   			WHEN   clutch_size = 0  
				   THEN wgt_female_size1
				ELSE 0
				END))  wgt_female_size1_immature,	
	   (sum(CASE
	   			WHEN   clutch_size >= 1  
				   THEN wgt_female_size1
				ELSE 0
				END))  wgt_female_size1_mature
    from cb_weight_grams_female
    where species_code = 68560
		 --and width <> 999
	     --and hauljoin(+) = h.hauljoin
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code,
				size1;

-- combine male and female weight tables

drop table cb_weight_grams_size1;

create table cb_weight_grams_size1 
( HAULJOIN                        NUMBER(12),
  VESSEL                          NUMBER(4),
  CRUISE                          NUMBER(6),
  HAUL                            NUMBER(4),
  GIS_STATION                     VARCHAR2(10),
  SPECIES_CODE                    NUMBER(6),
  SIZE1                           NUMBER,
  SHELL_CONDITION                 NUMBER,
  WGT_MALE_SIZE1                  NUMBER,
  WGT_FEMALE_SIZE1_IMMATURE       NUMBER,
  WGT_FEMALE_SIZE1_MATURE         NUMBER
);
insert into cb_weight_grams_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
wgt_male_size1,null,null
from cb_weight_grams_male;

insert into cb_weight_grams_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,wgt_female_size1_immature,wgt_female_size1_mature
from cb_weight_grams_matfem;

-- convert to metric tons

drop table cb_weight_mt_size1;

create table cb_weight_mt_size1 as
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
(wgt_male_size1 * 0.000001) mt_male_size1,
(wgt_female_size1_immature * 0.000001) mt_female_size1_immature,
(wgt_female_size1_mature * 0.000001) mt_female_size1_mature
from cb_weight_grams_size1
order by cruise,vessel,haul,gis_station,size1;

-- Combine the male, female, and unsexed by number tables

drop table cb_number_size1;

create table cb_number_size1 
( HAULJOIN                        NUMBER(12),
  VESSEL                          NUMBER(4),
  CRUISE                          NUMBER(6),
  HAUL                            NUMBER(4),
  GIS_STATION                     VARCHAR2(10),
  SPECIES_CODE                    NUMBER(6),
  SIZE1                           NUMBER,
  SHELL_CONDITION                 NUMBER,
  NUMBER_MALE_SIZE1               NUMBER,
  NUMBER_FEMALE_SIZE1_IMMATURE    NUMBER,
  NUMBER_FEMALE_SIZE1_MATURE      NUMBER,
  NUMBER_UNSEXED_SIZE1            NUMBER
);
insert into cb_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
number_male_size1,null,null,null
from cb_number_size1_male;

insert into cb_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,number_female_size1_immature,number_female_size1_mature,null
from cb_number_size1_matfem;

insert into cb_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,null,null,number_unsexed_size1
from cb_number_size1_unsexed;

-- This section sums the bairdi Tanner crab catch records by haul, sex,
-- and 1-mm size group.  

drop table cb_number_sizegroup;

create table cb_number_sizegroup as
select hauljoin, vessel, cruise, haul, gis_station, species_code, 
	   
	   (sum(CASE
	   			WHEN  size1 between 0 and 94.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le94,
     (sum(CASE
	   			WHEN  size1 between 0 and 102.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le102,	                  
				
	   (sum(CASE
	   			WHEN  size1 between 0 and 109.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le109,	
				
	   (sum(CASE
	   			WHEN  size1 between 0 and 112.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le112,	          
	   
	   (sum(CASE
	   			WHEN  size1 between 0 and 119.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le119,
        
	   (sum(CASE
	   			WHEN  size1 between 103.0 and 124.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_103to124,	        
	   
	   (sum(CASE
	   			WHEN  size1 between 113.0 and 124.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_113to124,	
		
	   (sum(CASE
	   			WHEN  size1 between 103.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge103,	                
		
	   (sum(CASE
	   			WHEN  size1 between 113.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge113,	        
		
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge120,	
	   
	   (sum(CASE
	   			WHEN  size1 between 125.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge125,        
	   
	   (sum(CASE
	   			WHEN  size1 between 110.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge110,
	   
	   (sum(CASE
	   			WHEN  size1 between 138.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge138,
	   
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_total,		
	   sum(number_female_size1_immature) number_female_immature,
     sum(number_female_size1_mature) number_female_mature,
     (sum(number_female_size1_immature)+ sum(number_female_size1_mature)) number_female_total,
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN number_unsexed_size1
				ELSE 0
				END))  number_unsexed_total														
	   from cb_number_size1
         where species_code = 68560
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code;
				

drop table cb_weight_mt_sizegroup;

create table cb_weight_mt_sizegroup as
select hauljoin, vessel, cruise, haul, gis_station, species_code, 
	   (sum(CASE
	   			WHEN  size1 between 0 and 94.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le94,
	   (sum(CASE
	   			WHEN  size1 between 0 and 102.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le102,	        
	   (sum(CASE
	   			WHEN  size1 between 0 and 109.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le109,	
	   (sum(CASE
	   			WHEN  size1 between 0 and 112.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le112,	          
	   (sum(CASE
	   			WHEN  size1 between 0 and 119.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le119,
	   (sum(CASE
	   			WHEN  size1 between 103.0 and 124.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_103to124,	        
	   (sum(CASE
	   			WHEN  size1 between 113.0 and 124.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_113to124,	
 	   (sum(CASE
	   			WHEN  size1 between 103.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge103,               
	   (sum(CASE
	   			WHEN  size1 between 113.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge113,        
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge120,
	   (sum(CASE
	   			WHEN  size1 between 125.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge125,        
	   (sum(CASE
	   			WHEN  size1 between 110.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge110,        
	   (sum(CASE
	   			WHEN  size1 between 138.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge138,
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_total,		
	   sum(mt_female_size1_immature) mt_female_immature,
     sum(mt_female_size1_mature) mt_female_mature,
     (sum(mt_female_size1_immature)+ sum(mt_female_size1_mature)) mt_female_total
	   from cb_weight_mt_size1
         where species_code = 68560
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code;
				
				

-- This section combines the haul and catch data, including
-- those haul/size groups where there was no catch.				

drop table cb_num_sizegroup_union;

create table cb_num_sizegroup_union as
select h.hauljoin,h.vessel,h.cruise,h.haul,h.gis_station,survey_year,
nvl(species_code,68560) species_code,
nvl(number_male_le94,0) number_male_le94,
nvl(number_male_le102,0) number_male_le102,
nvl(number_male_le109,0) number_male_le109,
nvl(number_male_le112,0) number_male_le112,
nvl(number_male_le119,0) number_male_le119,
nvl(number_male_103to124,0) number_male_103to124,
nvl(number_male_113to124,0) number_male_113to124,
nvl(number_male_ge103,0) number_male_ge103,
nvl(number_male_ge113,0) number_male_ge113,
nvl(number_male_ge120,0) number_male_ge120,
nvl(number_male_ge125,0) number_male_ge125,
nvl(number_male_ge110,0) number_male_ge110,
nvl(number_male_ge138,0) number_male_ge138,
nvl(number_male_total,0) number_male_total,
nvl(number_female_immature,0) number_female_immature,
nvl(number_female_mature,0) number_female_mature,
nvl(number_female_total,0) number_female_total,
nvl(number_unsexed_total,0) number_unsexed_total
from haul_newtimeseries_noretow h full outer join cb_number_sizegroup c
on h.hauljoin = c.hauljoin
where haul_type <> 17;

--  Similarly, by weight.

drop table cb_wgt_sizegroup_union;

create table cb_wgt_sizegroup_union as
select h.hauljoin,h.vessel,h.cruise,h.haul,h.gis_station,survey_year,
nvl(species_code,68560) species_code,
nvl(mt_male_le94,0) mt_male_le94,
nvl(mt_male_le102,0) mt_male_le102,
nvl(mt_male_le109,0) mt_male_le109,
nvl(mt_male_le112,0) mt_male_le112,
nvl(mt_male_le119,0) mt_male_le119,
nvl(mt_male_103to124,0) mt_male_103to124,
nvl(mt_male_113to124,0) mt_male_113to124,
nvl(mt_male_ge103,0) mt_male_ge103,
nvl(mt_male_ge113,0) mt_male_ge113,
nvl(mt_male_ge120,0) mt_male_ge120,
nvl(mt_male_ge125,0) mt_male_ge125,
nvl(mt_male_ge110,0) mt_male_ge110,
nvl(mt_male_ge138,0) mt_male_ge138,
nvl(mt_male_total,0) mt_male_total,
nvl(mt_female_immature,0) mt_female_immature,
nvl(mt_female_mature,0) mt_female_mature,
nvl(mt_female_total,0) mt_female_total
from haul_newtimeseries_noretow h full outer join cb_weight_mt_sizegroup c
on h.hauljoin = c.hauljoin
where haul_type <> 17;


-- This section calculates cpue for each haul.
-- If a station contains multiple tows, cpue
-- is calculated for each of the tows, not averaged for the station.
-- A value, even if 0 for no catch, is output for every size group,
-- every haul.  CPUE is calculated as number of crabs per square
-- nautical mile towed; area swept is the distance fished multiplied
-- by the actual (measured) net width.

drop table cb_cpuenum_sizegroup;

create table cb_cpuenum_sizegroup as
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,
c.gis_station,c.survey_year,c.species_code,h.gear_depth,h.surface_temperature,h.gear_temperature,
(number_male_le94 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le94,
(number_male_le102 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le102,
(number_male_le109 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le109,
(number_male_le112 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le112,
(number_male_le119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le119,
(number_male_103to124 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_103to124,
(number_male_113to124 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_113to124,
(number_male_ge103 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge103,
(number_male_ge113 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge113,
(number_male_ge120 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge120,
(number_male_ge125 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge125,
(number_male_ge110 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge110,
(number_male_ge138 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge138,
(number_male_total / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_total,
(number_female_immature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_immature,
(number_female_mature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_mature,
(number_female_total / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_total,
(number_unsexed_total / (((net_width/1000) * distance_fished) * 0.29155335)) unsexed_cpuenum_total
from cb_num_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin
and haul_type <> 17;


-- This section calculates cpue by weight for each haul.
-- If a station contains multiple tows, cpue is calculated 
-- for each of the tows, not averaged for the station.
-- A value, even if 0 for no catch, is output for every size group,
-- every haul.  CPUE is calculated as weight of crabs (already converted to metric tons) 
-- per square nautical mile towed; area swept is the distance fished multiplied
-- by the actual (measured) net width.

drop table cb_cpuewgt_sizegroup;

create table cb_cpuewgt_sizegroup as
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,
c.gis_station,c.survey_year,c.species_code,h.gear_depth,h.surface_temperature,h.gear_temperature,
(mt_male_le94 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le94,
(mt_male_le102 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le102,
(mt_male_le109 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le109,
(mt_male_le112 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le112,
(mt_male_le119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le119,
(mt_male_103to124 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_103to124,
(mt_male_113to124 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_113to124,
(mt_male_ge103 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge103,
(mt_male_ge113 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge113,
(mt_male_ge120 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge120,
(mt_male_ge125 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge125,
(mt_male_ge110 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge110,
(mt_male_ge138 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge138,
(mt_male_total / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_total,
(mt_female_immature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_immature,
(mt_female_mature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_mature,
(mt_female_total / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_total
from cb_wgt_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin
and haul_type <> 17;


drop table cb_male_cpuenum_weight_temp;

create table cb_male_cpuenum_weight_temp as 
select survey_year,cruise,
sum(gear_temperature * male_cpuenum_total)/sum(male_cpuenum_total) male_weighted_gear_temp
from cb_cpuenum_sizegroup
group by survey_year,cruise;

drop table cb_female_cpuenum_weight_temp;

create table cb_female_cpuenum_weight_temp as 
select survey_year,cruise,
sum(gear_temperature * female_cpuenum_immature)/sum(female_cpuenum_immature) immature_female_weighted_gear_temp,
sum(gear_temperature * female_cpuenum_mature)/sum(female_cpuenum_mature) mature_female_weighted_gear_temp,
sum(gear_temperature * female_cpuenum_total)/sum(female_cpuenum_total) total_female_weighted_gear_temp
from cb_cpuenum_sizegroup
group by survey_year,cruise;
