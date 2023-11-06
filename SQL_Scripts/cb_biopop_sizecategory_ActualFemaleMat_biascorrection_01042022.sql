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
select c.hauljoin,c.vessel,c.cruise,c.haul,c.gis_station,c.survey_year,c.species_code,
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
c.gis_station,c.survey_year,c.species_code,
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


drop table cb_meancpuenum_sizegroup;

create table cb_meancpuenum_sizegroup as
select c.survey_year,district,
AVG (male_cpuenum_le94) meancpuenum_male_le94,
AVG (male_cpuenum_le102) meancpuenum_male_le102,
AVG (male_cpuenum_le109) meancpuenum_male_le109,
AVG (male_cpuenum_le112) meancpuenum_male_le112,
AVG (male_cpuenum_le119) meancpuenum_male_le119,
AVG (male_cpuenum_103to124) meancpuenum_male_103to124,
AVG (male_cpuenum_113to124) meancpuenum_male_113to124,
AVG (male_cpuenum_ge103) meancpuenum_male_ge103,
AVG (male_cpuenum_ge113) meancpuenum_male_ge113,
AVG (male_cpuenum_ge120) meancpuenum_male_ge120,
AVG (male_cpuenum_ge125) meancpuenum_male_ge125,
AVG (male_cpuenum_ge110) meancpuenum_male_ge110,
AVG (male_cpuenum_ge138) meancpuenum_male_ge138,
AVG (male_cpuenum_total) meancpuenum_male_total,
AVG (female_cpuenum_immature) meancpuenum_female_immature,
AVG (female_cpuenum_mature) meancpuenum_female_mature,
AVG (female_cpuenum_total) meancpuenum_female_total,
AVG (unsexed_cpuenum_total) meancpuenum_unsexed_total,
AVG (male_cpuenum_total + female_cpuenum_total + unsexed_cpuenum_total) meancpuenum_gtotal
from cb_cpuenum_sizegroup c, strata_bairdi_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;

drop table cb_meancpuewgt_sizegroup;

create table cb_meancpuewgt_sizegroup as
select c.survey_year,district,
AVG (male_cpuewgt_le94) meancpuewgt_male_le94,
AVG (male_cpuewgt_le102) meancpuewgt_male_le102,
AVG (male_cpuewgt_le109) meancpuewgt_male_le109,
AVG (male_cpuewgt_le112) meancpuewgt_male_le112,
AVG (male_cpuewgt_le119) meancpuewgt_male_le119,
AVG (male_cpuewgt_103to124) meancpuewgt_male_103to124,
AVG (male_cpuewgt_113to124) meancpuewgt_male_113to124,
AVG (male_cpuewgt_ge103) meancpuewgt_male_ge103,
AVG (male_cpuewgt_ge113) meancpuewgt_male_ge113,
AVG (male_cpuewgt_ge120) meancpuewgt_male_ge120,
AVG (male_cpuewgt_ge125) meancpuewgt_male_ge125,
AVG (male_cpuewgt_ge110) meancpuewgt_male_ge110,
AVG (male_cpuewgt_ge138) meancpuewgt_male_ge138,
AVG (male_cpuewgt_total) meancpuewgt_male_total,
AVG (female_cpuewgt_immature) meancpuewgt_female_immature,
AVG (female_cpuewgt_mature) meancpuewgt_female_mature,
AVG (female_cpuewgt_total) meancpuewgt_female_total,
AVG (male_cpuewgt_total + female_cpuewgt_total) meancpuewgt_gtotal
from cb_cpuewgt_sizegroup c, strata_bairdi_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;


drop table cb_popbystratum_sizegroup;

create table cb_popbystratum_sizegroup as
select distinct c.survey_year,stratum,c.district,
 (meancpuenum_male_le94 * total_area) pop_male_le94,
 (meancpuenum_male_le102 * total_area) pop_male_le102,
 (meancpuenum_male_le109 * total_area) pop_male_le109,
 (meancpuenum_male_le112 * total_area) pop_male_le112,
 (meancpuenum_male_le119 * total_area) pop_male_le119,
 (meancpuenum_male_103to124 * total_area) pop_male_103to124,
 (meancpuenum_male_113to124 * total_area) pop_male_113to124,
 (meancpuenum_male_ge103 * total_area) pop_male_ge103,
 (meancpuenum_male_ge113 * total_area) pop_male_ge113,
 (meancpuenum_male_ge120 * total_area) pop_male_ge120,
 (meancpuenum_male_ge125 * total_area) pop_male_ge125,
 (meancpuenum_male_ge110 * total_area) pop_male_ge110,
 (meancpuenum_male_ge138 * total_area) pop_male_ge138,
 (meancpuenum_male_total * total_area) pop_male_total,
 (meancpuenum_female_immature * total_area) pop_female_immature,
 (meancpuenum_female_mature * total_area) pop_female_mature,
 (meancpuenum_female_total * total_area) pop_female_total,
 (meancpuenum_unsexed_total * total_area) pop_unsexed_total,
 (meancpuenum_gtotal * total_area) pop_gtotal
from cb_meancpuenum_sizegroup c, strata_bairdi_newtimeseries s
where c.district = s.district
and c.survey_year = s.survey_year
order by survey_year,district;

drop table cb_biobystratum_sizegroup;

create table cb_biobystratum_sizegroup as
select distinct c.survey_year,stratum,c.district,
(meancpuewgt_male_le94 * total_area) bio_male_le94,
(meancpuewgt_male_le102 * total_area) bio_male_le102,
(meancpuewgt_male_le109 * total_area) bio_male_le109,
(meancpuewgt_male_le112 * total_area) bio_male_le112,
(meancpuewgt_male_le119 * total_area) bio_male_le119,
(meancpuewgt_male_103to124 * total_area) bio_male_103to124,
(meancpuewgt_male_113to124 * total_area) bio_male_113to124,
(meancpuewgt_male_ge103 * total_area) bio_male_ge103,
(meancpuewgt_male_ge113 * total_area) bio_male_ge113,
(meancpuewgt_male_ge120 * total_area) bio_male_ge120,
(meancpuewgt_male_ge125 * total_area) bio_male_ge125,
(meancpuewgt_male_ge110 * total_area) bio_male_ge110,
(meancpuewgt_male_ge138 * total_area) bio_male_ge138,
(meancpuewgt_male_total * total_area) bio_male_total,
(meancpuewgt_female_immature * total_area) bio_female_immature,
(meancpuewgt_female_mature * total_area) bio_female_mature,
(meancpuewgt_female_total * total_area) bio_female_total,
(meancpuewgt_gtotal * total_area) bio_gtotal
from cb_meancpuewgt_sizegroup c, strata_bairdi_newtimeseries s
where c.district = s.district
and c.survey_year = s.survey_year
order by survey_year,district;


drop table cb_varcpuenum_sizegroup;

create table cb_varcpuenum_sizegroup as
select c.survey_year,district,
VARIANCE (male_cpuenum_le94) varcpuenum_male_le94,
VARIANCE (male_cpuenum_le102) varcpuenum_male_le102,
VARIANCE (male_cpuenum_le109) varcpuenum_male_le109,
VARIANCE (male_cpuenum_le112) varcpuenum_male_le112,
VARIANCE (male_cpuenum_le119) varcpuenum_male_le119,
VARIANCE (male_cpuenum_103to124) varcpuenum_male_103to124,
VARIANCE (male_cpuenum_113to124) varcpuenum_male_113to124,
VARIANCE (male_cpuenum_ge103) varcpuenum_male_ge103,
VARIANCE (male_cpuenum_ge113) varcpuenum_male_ge113,
VARIANCE (male_cpuenum_ge120) varcpuenum_male_ge120,
VARIANCE (male_cpuenum_ge125) varcpuenum_male_ge125,
VARIANCE (male_cpuenum_ge110) varcpuenum_male_ge110,
VARIANCE (male_cpuenum_ge138) varcpuenum_male_ge138,
VARIANCE (male_cpuenum_total) varcpuenum_male_total,
VARIANCE (female_cpuenum_immature) varcpuenum_female_immature,
VARIANCE (female_cpuenum_mature) varcpuenum_female_mature,
VARIANCE (female_cpuenum_total) varcpuenum_female_total,
VARIANCE (unsexed_cpuenum_total) varcpuenum_unsexed_total,
VARIANCE (male_cpuenum_total + female_cpuenum_total + unsexed_cpuenum_total) varcpuenum_gtotal
from cb_cpuenum_sizegroup c, strata_bairdi_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;

drop table cb_varcpuewgt_sizegroup;

create table cb_varcpuewgt_sizegroup as
select c.survey_year,district,
VARIANCE (male_cpuewgt_le94) varcpuewgt_male_le94,
VARIANCE (male_cpuewgt_le102) varcpuewgt_male_le102,
VARIANCE (male_cpuewgt_le109) varcpuewgt_male_le109,
VARIANCE (male_cpuewgt_le112) varcpuewgt_male_le112,
VARIANCE (male_cpuewgt_le119) varcpuewgt_male_le119,
VARIANCE (male_cpuewgt_103to124) varcpuewgt_male_103to124,
VARIANCE (male_cpuewgt_113to124) varcpuewgt_male_113to124,
VARIANCE (male_cpuewgt_ge103) varcpuewgt_male_ge103,
VARIANCE (male_cpuewgt_ge113) varcpuewgt_male_ge113,
VARIANCE (male_cpuewgt_ge120) varcpuewgt_male_ge120,
VARIANCE (male_cpuewgt_ge125) varcpuewgt_male_ge125,
VARIANCE (male_cpuewgt_ge110) varcpuewgt_male_ge110,
VARIANCE (male_cpuewgt_ge138) varcpuewgt_male_ge138,
VARIANCE (male_cpuewgt_total) varcpuewgt_male_total,
VARIANCE (female_cpuewgt_immature) varcpuewgt_female_immature,
VARIANCE (female_cpuewgt_mature) varcpuewgt_female_mature,
VARIANCE (female_cpuewgt_total) varcpuewgt_female_total,
VARIANCE (male_cpuewgt_total + female_cpuewgt_total) varcpuewgt_gtotal
from cb_cpuewgt_sizegroup c, strata_bairdi_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;


drop table cb_haulcount;

create table cb_haulcount as
select count(hauljoin)number_tows, h.survey_year, district
from haul_newtimeseries_noretow h, strata_bairdi_newtimeseries s
where h.gis_station = s.station_id
and h.survey_year = s.survey_year
and haul_type <> 17
group by h.survey_year, district;

drop table cb_variancepop_sizegroup;

create table cb_variancepop_sizegroup as
select distinct c.survey_year,stratum,c.district,
((varcpuenum_male_le94 * (power(total_area,2)))/number_tows) varpop_male_le94,
((varcpuenum_male_le102 * (power(total_area,2)))/number_tows) varpop_male_le102,
((varcpuenum_male_le109 * (power(total_area,2)))/number_tows) varpop_male_le109,
((varcpuenum_male_le112 * (power(total_area,2)))/number_tows) varpop_male_le112,
((varcpuenum_male_le119 * (power(total_area,2)))/number_tows) varpop_male_le119,
((varcpuenum_male_103to124 * (power(total_area,2)))/number_tows) varpop_male_103to124,
((varcpuenum_male_113to124 * (power(total_area,2)))/number_tows) varpop_male_113to124,
((varcpuenum_male_ge103 * (power(total_area,2)))/number_tows) varpop_male_ge103,
((varcpuenum_male_ge113 * (power(total_area,2)))/number_tows) varpop_male_ge113,
((varcpuenum_male_ge120 * (power(total_area,2)))/number_tows) varpop_male_ge120,
((varcpuenum_male_ge125 * (power(total_area,2)))/number_tows) varpop_male_ge125,
((varcpuenum_male_ge110 * (power(total_area,2)))/number_tows) varpop_male_ge110,
((varcpuenum_male_ge138 * (power(total_area,2)))/number_tows) varpop_male_ge138,
((varcpuenum_male_total * (power(total_area,2)))/number_tows) varpop_male_total,
((varcpuenum_female_immature * (power(total_area,2)))/number_tows) varpop_female_immature,
((varcpuenum_female_mature * (power(total_area,2)))/number_tows) varpop_female_mature,
((varcpuenum_female_total * (power(total_area,2)))/number_tows) varpop_female_total,
((varcpuenum_unsexed_total * (power(total_area,2)))/number_tows) varpop_unsexed_total,
((varcpuenum_gtotal * (power(total_area,2)))/number_tows) varpop_gtotal
from strata_bairdi_newtimeseries s, cb_varcpuenum_sizegroup c, cb_haulcount n
where c.district = s.district
and c.district = n.district
and c.survey_year = s.survey_year
and c.survey_year = n.survey_year
order by c.survey_year,stratum;

drop table cb_variancebio_sizegroup;

create table cb_variancebio_sizegroup as
select distinct c.survey_year,stratum,c.district,
((varcpuewgt_male_le94 * (power(total_area,2)))/number_tows) varbio_male_le94,
((varcpuewgt_male_le102 * (power(total_area,2)))/number_tows) varbio_male_le102,
((varcpuewgt_male_le109 * (power(total_area,2)))/number_tows) varbio_male_le109,
((varcpuewgt_male_le112 * (power(total_area,2)))/number_tows) varbio_male_le112,
((varcpuewgt_male_le119 * (power(total_area,2)))/number_tows) varbio_male_le119,
((varcpuewgt_male_103to124 * (power(total_area,2)))/number_tows) varbio_male_103to124,
((varcpuewgt_male_113to124 * (power(total_area,2)))/number_tows) varbio_male_113to124,
((varcpuewgt_male_ge103 * (power(total_area,2)))/number_tows) varbio_male_ge103,
((varcpuewgt_male_ge113 * (power(total_area,2)))/number_tows) varbio_male_ge113,
((varcpuewgt_male_ge120 * (power(total_area,2)))/number_tows) varbio_male_ge120,
((varcpuewgt_male_ge125 * (power(total_area,2)))/number_tows) varbio_male_ge125,
((varcpuewgt_male_ge110 * (power(total_area,2)))/number_tows) varbio_male_ge110,
((varcpuewgt_male_ge138 * (power(total_area,2)))/number_tows) varbio_male_ge138,
((varcpuewgt_male_total * (power(total_area,2)))/number_tows) varbio_male_total,
((varcpuewgt_female_immature * (power(total_area,2)))/number_tows) varbio_female_immature,
((varcpuewgt_female_mature * (power(total_area,2)))/number_tows) varbio_female_mature,
((varcpuewgt_female_total * (power(total_area,2)))/number_tows) varbio_female_total,
((varcpuewgt_gtotal * (power(total_area,2)))/number_tows) varbio_gtotal
from strata_bairdi_newtimeseries s, cb_varcpuewgt_sizegroup c, cb_haulcount n
where c.district = s.district
and c.district = n.district
and c.survey_year = s.survey_year
and c.survey_year = n.survey_year
order by c.survey_year,stratum;


-- Calculation by stock or district from this point on
-- For bairdi, this will be total, east of 166W, and west of 166W

drop table cb_popall_sizegroup;

create table cb_popall_sizegroup as
select survey_year,
sum(pop_male_le94) sum_pop_male_le94,
sum(pop_male_le102) sum_pop_male_le102,
sum(pop_male_le109) sum_pop_male_le109,
sum(pop_male_le112) sum_pop_male_le112,
sum(pop_male_le119) sum_pop_male_le119,
sum(pop_male_103to124) sum_pop_male_103to124,
sum(pop_male_113to124) sum_pop_male_113to124,
sum(pop_male_ge103) sum_pop_male_ge103,
sum(pop_male_ge113) sum_pop_male_ge113,
sum(pop_male_ge120) sum_pop_male_ge120,
sum(pop_male_ge125) sum_pop_male_ge125,
sum(pop_male_ge110) sum_pop_male_ge110,
sum(pop_male_ge138) sum_pop_male_ge138,
sum(pop_male_total) sum_pop_male_total,
sum(pop_female_immature) sum_pop_female_immature,
sum(pop_female_mature) sum_pop_female_mature,
sum(pop_female_total) sum_pop_female_total,
sum(pop_unsexed_total) sum_pop_unsexed_total,
sum(pop_gtotal) sum_pop_gtotal
from cb_popbystratum_sizegroup
group by survey_year
order by survey_year;

drop table cb_bioall_sizegroup;

create table cb_bioall_sizegroup as
select survey_year,
sum(bio_male_le94) sum_bio_male_le94,
sum(bio_male_le102) sum_bio_male_le102,
sum(bio_male_le109) sum_bio_male_le109,
sum(bio_male_le112) sum_bio_male_le112,
sum(bio_male_le119) sum_bio_male_le119,
sum(bio_male_103to124) sum_bio_male_103to124,
sum(bio_male_113to124) sum_bio_male_113to124,
sum(bio_male_ge103) sum_bio_male_ge103,
sum(bio_male_ge113) sum_bio_male_ge113,
sum(bio_male_ge120) sum_bio_male_ge120,
sum(bio_male_ge125) sum_bio_male_ge125,
sum(bio_male_ge110) sum_bio_male_ge110,
sum(bio_male_ge138) sum_bio_male_ge138,
sum(bio_male_total) sum_bio_male_total,
sum(bio_female_immature) sum_bio_female_immature,
sum(bio_female_mature) sum_bio_female_mature,
sum(bio_female_total) sum_bio_female_total,
sum(bio_gtotal) sum_bio_gtotal
from cb_biobystratum_sizegroup
group by survey_year
order by survey_year;


drop table cb_varpop_sizegroup_sum;

create table cb_varpop_sizegroup_sum as
select distinct survey_year,
sum(varpop_male_le94) sum_varpop_male_le94,
sum(varpop_male_le102) sum_varpop_male_le102,
sum(varpop_male_le109) sum_varpop_male_le109,
sum(varpop_male_le112) sum_varpop_male_le112,
sum(varpop_male_le119) sum_varpop_male_le119,
sum(varpop_male_103to124) sum_varpop_male_103to124,
sum(varpop_male_113to124) sum_varpop_male_113to124,
sum(varpop_male_ge103) sum_varpop_male_ge103,
sum(varpop_male_ge113) sum_varpop_male_ge113,
sum(varpop_male_ge120) sum_varpop_male_ge120,
sum(varpop_male_ge125) sum_varpop_male_ge125,
sum(varpop_male_ge110) sum_varpop_male_ge110,
sum(varpop_male_ge138) sum_varpop_male_ge138,
sum(varpop_male_total) sum_varpop_male_total,
sum(varpop_female_immature) sum_varpop_female_immature,
sum(varpop_female_mature) sum_varpop_female_mature,
sum(varpop_female_total) sum_varpop_female_total,
sum(varpop_unsexed_total) sum_varpop_unsexed_total,
sum(varpop_gtotal) sum_varpop_gtotal
from cb_variancepop_sizegroup
group by survey_year
order by survey_year;

drop table cb_varbio_sizegroup_sum;

create table cb_varbio_sizegroup_sum as
select distinct survey_year,
sum(varbio_male_le94) sum_varbio_male_le94,
sum(varbio_male_le102) sum_varbio_male_le102,
sum(varbio_male_le109) sum_varbio_male_le109,
sum(varbio_male_le112) sum_varbio_male_le112,
sum(varbio_male_le119) sum_varbio_male_le119,
sum(varbio_male_103to124) sum_varbio_male_103to124,
sum(varbio_male_113to124) sum_varbio_male_113to124,
sum(varbio_male_ge103) sum_varbio_male_ge103,
sum(varbio_male_ge113) sum_varbio_male_ge113,
sum(varbio_male_ge120) sum_varbio_male_ge120,
sum(varbio_male_ge125) sum_varbio_male_ge125,
sum(varbio_male_ge110) sum_varbio_male_ge110,
sum(varbio_male_ge138) sum_varbio_male_ge138,
sum(varbio_male_total) sum_varbio_male_total,
sum(varbio_female_immature) sum_varbio_female_immature,
sum(varbio_female_mature) sum_varbio_female_mature,
sum(varbio_female_total) sum_varbio_female_total,
sum(varbio_gtotal) sum_varbio_gtotal
from cb_variancebio_sizegroup
group by survey_year
order by survey_year;

drop table cb_pop_sizegroup_cv;

create table cb_pop_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_pop_male_le94 <> 0
	 then ((sqrt(sum_varpop_male_le94))/sum_pop_male_le94)
	 else 0
	 end) cv_pop_male_le94,
(CASE
	 when sum_pop_male_le102 <> 0
	 then ((sqrt(sum_varpop_male_le102))/sum_pop_male_le102)
	 else 0
	 end) cv_pop_male_le102,   
(CASE
	 when sum_pop_male_le109 <> 0
	 then ((sqrt(sum_varpop_male_le109))/sum_pop_male_le109)
	 else 0
	 end) cv_pop_male_le109,
(CASE
	 when sum_pop_male_le112 <> 0
	 then ((sqrt(sum_varpop_male_le112))/sum_pop_male_le112)
	 else 0
	 end) cv_pop_male_le112,
(CASE
	 when sum_pop_male_le119 <> 0
	 then ((sqrt(sum_varpop_male_le119))/sum_pop_male_le119)
	 else 0
	 end) cv_pop_male_le119,
(CASE
	 when sum_pop_male_103to124 <> 0
	 then ((sqrt(sum_varpop_male_103to124))/sum_pop_male_103to124)
	 else 0
	 end) cv_pop_male_103to124,   
(CASE
	 when sum_pop_male_113to124 <> 0
	 then ((sqrt(sum_varpop_male_113to124))/sum_pop_male_113to124)
	 else 0
	 end) cv_pop_male_113to124,
(CASE
	 when sum_pop_male_ge103 <> 0
	 then ((sqrt(sum_varpop_male_ge103))/sum_pop_male_ge103)
	 else 0
	 end) cv_pop_male_ge103,	       
(CASE
	 when sum_pop_male_ge113 <> 0
	 then ((sqrt(sum_varpop_male_ge113))/sum_pop_male_ge113)
	 else 0
	 end) cv_pop_male_ge113,	    
(CASE
	 when sum_pop_male_ge120 <> 0
	 then ((sqrt(sum_varpop_male_ge120))/sum_pop_male_ge120)
	 else 0
	 end) cv_pop_male_ge120,	 
(CASE
	 when sum_pop_male_ge125 <> 0
	 then ((sqrt(sum_varpop_male_ge125))/sum_pop_male_ge125)
	 else 0
	 end) cv_pop_male_ge125,	 	    
(CASE
	 when sum_pop_male_ge110 <> 0
	 then ((sqrt(sum_varpop_male_ge110))/sum_pop_male_ge110)
	 else 0
	 end) cv_pop_male_ge110,	 	 
(CASE
	 when sum_pop_male_ge138 <> 0
	 then ((sqrt(sum_varpop_male_ge138))/sum_pop_male_ge138)
	 else 0
	 end) cv_pop_male_ge138,
(CASE
	 when sum_pop_male_total <> 0
	 then ((sqrt(sum_varpop_male_total))/sum_pop_male_total)
	 else 0
	 end) cv_pop_male_total,
(CASE
	 when sum_pop_female_immature <> 0
	 then ((sqrt(sum_varpop_female_immature))/sum_pop_female_immature)
	 else 0
	 end) cv_pop_female_immature,	     
(CASE
	 when sum_pop_female_mature <> 0
	 then ((sqrt(sum_varpop_female_mature))/sum_pop_female_mature)
	 else 0
	 end) cv_pop_female_mature,	  
(CASE
	 when sum_pop_female_total <> 0
	 then ((sqrt(sum_varpop_female_total))/sum_pop_female_total)
	 else 0
	 end) cv_pop_female_total,
(CASE
	 when sum_pop_unsexed_total <> 0
	 then ((sqrt(sum_varpop_unsexed_total))/sum_pop_unsexed_total)
	 else 0
	 end) cv_pop_unsexed_total,
(CASE
	 when sum_pop_gtotal <> 0
	 then ((sqrt(sum_varpop_gtotal))/sum_pop_gtotal)
	 else 0
	 end) cv_pop_gtotal	  	 	 	 	  
from cb_varpop_sizegroup_sum a, cb_popall_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;	 	 	 

drop table cb_bio_sizegroup_cv;

create table cb_bio_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_bio_male_le94 <> 0
	 then ((sqrt(sum_varbio_male_le94))/sum_bio_male_le94)
	 else 0
	 end) cv_bio_male_le94,
(CASE
	 when sum_bio_male_le102 <> 0
	 then ((sqrt(sum_varbio_male_le102))/sum_bio_male_le102)
	 else 0
	 end) cv_bio_male_le102,   
(CASE
	 when sum_bio_male_le109 <> 0
	 then ((sqrt(sum_varbio_male_le109))/sum_bio_male_le109)
	 else 0
	 end) cv_bio_male_le109,
(CASE
	 when sum_bio_male_le112 <> 0
	 then ((sqrt(sum_varbio_male_le112))/sum_bio_male_le112)
	 else 0
	 end) cv_bio_male_le112,
(CASE
	 when sum_bio_male_le119 <> 0
	 then ((sqrt(sum_varbio_male_le119))/sum_bio_male_le119)
	 else 0
	 end) cv_bio_male_le119,
(CASE
	 when sum_bio_male_103to124 <> 0
	 then ((sqrt(sum_varbio_male_103to124))/sum_bio_male_103to124)
	 else 0
	 end) cv_bio_male_103to124,
(CASE
	 when sum_bio_male_113to124 <> 0
	 then ((sqrt(sum_varbio_male_113to124))/sum_bio_male_113to124)
	 else 0
	 end) cv_bio_male_113to124,   
(CASE
	 when sum_bio_male_ge103 <> 0
	 then ((sqrt(sum_varbio_male_ge103))/sum_bio_male_ge103)
	 else 0
	 end) cv_bio_male_ge103,	       
(CASE
	 when sum_bio_male_ge113 <> 0
	 then ((sqrt(sum_varbio_male_ge113))/sum_bio_male_ge113)
	 else 0
	 end) cv_bio_male_ge113,	    
(CASE
	 when sum_bio_male_ge120 <> 0
	 then ((sqrt(sum_varbio_male_ge120))/sum_bio_male_ge120)
	 else 0
	 end) cv_bio_male_ge120,	 
(CASE
	 when sum_bio_male_ge125 <> 0
	 then ((sqrt(sum_varbio_male_ge125))/sum_bio_male_ge125)
	 else 0
	 end) cv_bio_male_ge125,	 	    
(CASE
	 when sum_bio_male_ge110 <> 0
	 then ((sqrt(sum_varbio_male_ge110))/sum_bio_male_ge110)
	 else 0
	 end) cv_bio_male_ge110,	 	 
(CASE
	 when sum_bio_male_ge138 <> 0
	 then ((sqrt(sum_varbio_male_ge138))/sum_bio_male_ge138)
	 else 0
	 end) cv_bio_male_ge138,
(CASE
	 when sum_bio_male_total <> 0
	 then ((sqrt(sum_varbio_male_total))/sum_bio_male_total)
	 else 0
	 end) cv_bio_male_total,
(CASE
	 when sum_bio_female_immature <> 0
	 then ((sqrt(sum_varbio_female_immature))/sum_bio_female_immature)
	 else 0
	 end) cv_bio_female_immature,	     
(CASE
	 when sum_bio_female_mature <> 0
	 then ((sqrt(sum_varbio_female_mature))/sum_bio_female_mature)
	 else 0
	 end) cv_bio_female_mature,	  
(CASE
	 when sum_bio_female_total <> 0
	 then ((sqrt(sum_varbio_female_total))/sum_bio_female_total)
	 else 0
	 end) cv_bio_female_total,
(CASE
	 when sum_bio_gtotal <> 0
	 then ((sqrt(sum_varbio_gtotal))/sum_bio_gtotal)
	 else 0
	 end) cv_bio_gtotal	  	 	 	 	  
from cb_varbio_sizegroup_sum a, cb_bioall_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;


-- CI calcs

create or replace view cb_sizegroup_stderr_pop as
select distinct survey_year,
(sqrt(sum_varpop_male_le94)) stderr_pop_male_le94,
(sqrt(sum_varpop_male_le102)) stderr_pop_male_le102,
(sqrt(sum_varpop_male_le109)) stderr_pop_male_le109,
(sqrt(sum_varpop_male_le112)) stderr_pop_male_le112,
(sqrt(sum_varpop_male_le119)) stderr_pop_male_le119,
(sqrt(sum_varpop_male_103to124)) stderr_pop_male_103to124,
(sqrt(sum_varpop_male_113to124)) stderr_pop_male_113to124,
(sqrt(sum_varpop_male_ge103)) stderr_pop_male_ge103,
(sqrt(sum_varpop_male_ge113)) stderr_pop_male_ge113,
(sqrt(sum_varpop_male_ge120)) stderr_pop_male_ge120,
(sqrt(sum_varpop_male_ge125)) stderr_pop_male_ge125,
(sqrt(sum_varpop_male_ge110)) stderr_pop_male_ge110,
(sqrt(sum_varpop_male_ge138)) stderr_pop_male_ge138,
(sqrt(sum_varpop_male_total)) stderr_pop_male_total,
(sqrt(sum_varpop_female_immature)) stderr_pop_female_immature,
(sqrt(sum_varpop_female_mature)) stderr_pop_female_mature,
(sqrt(sum_varpop_female_total)) stderr_pop_female_total,
(sqrt(sum_varpop_unsexed_total)) stderr_pop_unsexed_total,
(sqrt(sum_varpop_gtotal)) stderr_pop_gtotal
from cb_varpop_sizegroup_sum;

create or replace view cb_sizegroup_stderr_bio as
select distinct survey_year,
(sqrt(sum_varbio_male_le94)) stderr_bio_male_le94,
(sqrt(sum_varbio_male_le102)) stderr_bio_male_le102,
(sqrt(sum_varbio_male_le109)) stderr_bio_male_le109,
(sqrt(sum_varbio_male_le112)) stderr_bio_male_le112,
(sqrt(sum_varbio_male_le119)) stderr_bio_male_le119,
(sqrt(sum_varbio_male_103to124)) stderr_bio_male_103to124,
(sqrt(sum_varbio_male_113to124)) stderr_bio_male_113to124,
(sqrt(sum_varbio_male_ge103)) stderr_bio_male_ge103,
(sqrt(sum_varbio_male_ge113)) stderr_bio_male_ge113,
(sqrt(sum_varbio_male_ge120)) stderr_bio_male_ge120,
(sqrt(sum_varbio_male_ge125)) stderr_bio_male_ge125,
(sqrt(sum_varbio_male_ge110)) stderr_bio_male_ge110,
(sqrt(sum_varbio_male_ge138)) stderr_bio_male_ge138,
(sqrt(sum_varbio_male_total)) stderr_bio_male_total,
(sqrt(sum_varbio_female_immature)) stderr_bio_female_immature,
(sqrt(sum_varbio_female_mature)) stderr_bio_female_mature,
(sqrt(sum_varbio_female_total)) stderr_bio_female_total,
(sqrt(sum_varbio_gtotal)) stderr_bio_gtotal
from cb_varbio_sizegroup_sum;

drop table cb_sizegroup_confidence_pop;

create table cb_sizegroup_confidence_pop as
select distinct survey_year,
(1.96 * stderr_pop_male_le94) ci_pop_male_le94,
(1.96 * stderr_pop_male_le102) ci_pop_male_le102,
(1.96 * stderr_pop_male_le109) ci_pop_male_le109,
(1.96 * stderr_pop_male_le112) ci_pop_male_le112,
(1.96 * stderr_pop_male_le119) ci_pop_male_le119,
(1.96 * stderr_pop_male_103to124) ci_pop_male_103to124,
(1.96 * stderr_pop_male_113to124) ci_pop_male_113to124,
(1.96 * stderr_pop_male_ge103) ci_pop_male_ge103,
(1.96 * stderr_pop_male_ge113) ci_pop_male_ge113,
(1.96 * stderr_pop_male_ge120) ci_pop_male_ge120,
(1.96 * stderr_pop_male_ge125) ci_pop_male_ge125,
(1.96 * stderr_pop_male_ge110) ci_pop_male_ge110,
(1.96 * stderr_pop_male_ge138) ci_pop_male_ge138,
(1.96 * stderr_pop_male_total) ci_pop_male_total,
(1.96 * stderr_pop_female_immature) ci_pop_female_immature,
(1.96 * stderr_pop_female_mature) ci_pop_female_mature,
(1.96 * stderr_pop_female_total) ci_pop_female_total,
(1.96 * stderr_pop_unsexed_total) ci_pop_unsexed_total,
(1.96 * stderr_pop_gtotal) ci_pop_gtotal
from cb_sizegroup_stderr_pop;

drop table cb_sizegroup_confidence_bio;

create table cb_sizegroup_confidence_bio as
select distinct survey_year,
((1.96 * stderr_bio_male_le94)) ci_bio_male_le94,
((1.96 * stderr_bio_male_le102)) ci_bio_male_le102,
((1.96 * stderr_bio_male_le109)) ci_bio_male_le109,
((1.96 * stderr_bio_male_le112)) ci_bio_male_le112,
((1.96 * stderr_bio_male_le119)) ci_bio_male_le119,
((1.96 * stderr_bio_male_103to124)) ci_bio_male_103to124,
((1.96 * stderr_bio_male_113to124)) ci_bio_male_113to124,
((1.96 * stderr_bio_male_ge103)) ci_bio_male_ge103,
((1.96 * stderr_bio_male_ge113)) ci_bio_male_ge113,
((1.96 * stderr_bio_male_ge120)) ci_bio_male_ge120,
((1.96 * stderr_bio_male_ge125)) ci_bio_male_ge125,
((1.96 * stderr_bio_male_ge110)) ci_bio_male_ge110,
((1.96 * stderr_bio_male_ge138)) ci_bio_male_ge138,
((1.96 * stderr_bio_male_total)) ci_bio_male_total,
((1.96 * stderr_bio_female_immature)) ci_bio_female_immature,
((1.96 * stderr_bio_female_mature)) ci_bio_female_mature,
((1.96 * stderr_bio_female_total)) ci_bio_female_total,
((1.96 * stderr_bio_gtotal)) ci_bio_gtotal
from cb_sizegroup_stderr_bio;



--  east of 166

drop table e166cb_pop_sizegroup;

create table e166cb_pop_sizegroup as
select survey_year,
sum(pop_male_le94) sum_pop_male_le94,
sum(pop_male_le109) sum_pop_male_le109,
sum(pop_male_le112) sum_pop_male_le112,
sum(pop_male_le119) sum_pop_male_le119,
sum(pop_male_113to124) sum_pop_male_113to124,
sum(pop_male_ge113) sum_pop_male_ge113,
sum(pop_male_ge120) sum_pop_male_ge120,
sum(pop_male_ge110) sum_pop_male_ge110,
sum(pop_male_ge125) sum_pop_male_ge125,
sum(pop_male_total) sum_pop_male_total,
sum(pop_female_immature) sum_pop_female_immature,
sum(pop_female_mature) sum_pop_female_mature,
sum(pop_female_total) sum_pop_female_total,
sum(pop_unsexed_total) sum_pop_unsexed_total,
sum(pop_gtotal) sum_pop_gtotal
from cb_popbystratum_sizegroup
where district = 'East 166'
group by survey_year
order by survey_year;

drop table e166cb_bio_sizegroup;

create table e166cb_bio_sizegroup as
select survey_year,
sum(bio_male_le94) sum_bio_male_le94,
sum(bio_male_le109) sum_bio_male_le109,
sum(bio_male_le112) sum_bio_male_le112,
sum(bio_male_le119) sum_bio_male_le119,
sum(bio_male_113to124) sum_bio_male_113to124,
sum(bio_male_ge113) sum_bio_male_ge113,
sum(bio_male_ge120) sum_bio_male_ge120,
sum(bio_male_ge110) sum_bio_male_ge110,
sum(bio_male_ge125) sum_bio_male_ge125,
sum(bio_male_total) sum_bio_male_total,
sum(bio_female_immature) sum_bio_female_immature,
sum(bio_female_mature) sum_bio_female_mature,
sum(bio_female_total) sum_bio_female_total,
sum(bio_gtotal) sum_bio_gtotal
from cb_biobystratum_sizegroup
where district = 'East 166'
group by survey_year
order by survey_year;


drop table e166cb_varpop_sizegroup_sum;

create table e166cb_varpop_sizegroup_sum as
select distinct survey_year,
sum(varpop_male_le94) sum_varpop_male_le94,
sum(varpop_male_le109) sum_varpop_male_le109,
sum(varpop_male_le112) sum_varpop_male_le112,
sum(varpop_male_le119) sum_varpop_male_le119,
sum(varpop_male_113to124) sum_varpop_male_113to124,
sum(varpop_male_ge113) sum_varpop_male_ge113,
sum(varpop_male_ge120) sum_varpop_male_ge120,
sum(varpop_male_ge110) sum_varpop_male_ge110,
sum(varpop_male_ge125) sum_varpop_male_ge125,
sum(varpop_male_total) sum_varpop_male_total,
sum(varpop_female_immature) sum_varpop_female_immature,
sum(varpop_female_mature) sum_varpop_female_mature,
sum(varpop_female_total) sum_varpop_female_total,
sum(varpop_unsexed_total) sum_varpop_unsexed_total,
sum(varpop_gtotal) sum_varpop_gtotal
from cb_variancepop_sizegroup
where district = 'East 166'
group by survey_year
order by survey_year;

drop table e166cb_varbio_sizegroup_sum;

create table e166cb_varbio_sizegroup_sum as
select distinct survey_year,
sum(varbio_male_le94) sum_varbio_male_le94,
sum(varbio_male_le109) sum_varbio_male_le109,
sum(varbio_male_le112) sum_varbio_male_le112,
sum(varbio_male_le119) sum_varbio_male_le119,
sum(varbio_male_113to124) sum_varbio_male_113to124,
sum(varbio_male_ge113) sum_varbio_male_ge113,
sum(varbio_male_ge120) sum_varbio_male_ge120,
sum(varbio_male_ge110) sum_varbio_male_ge110,
sum(varbio_male_ge125) sum_varbio_male_ge125,
sum(varbio_male_total) sum_varbio_male_total,
sum(varbio_female_immature) sum_varbio_female_immature,
sum(varbio_female_mature) sum_varbio_female_mature,
sum(varbio_female_total) sum_varbio_female_total,
sum(varbio_gtotal) sum_varbio_gtotal
from cb_variancebio_sizegroup
where district = 'East 166'
group by survey_year
order by survey_year;

drop table e166cb_pop_sizegroup_cv;

create table e166cb_pop_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_pop_male_le94 <> 0
	 then ((sqrt(sum_varpop_male_le94))/sum_pop_male_le94)
	 else 0
	 end) cv_pop_male_le94,
(CASE
	 when sum_pop_male_le109 <> 0
	 then ((sqrt(sum_varpop_male_le109))/sum_pop_male_le109)
	 else 0
	 end) cv_pop_male_le109,
(CASE
	 when sum_pop_male_le112 <> 0
	 then ((sqrt(sum_varpop_male_le112))/sum_pop_male_le112)
	 else 0
	 end) cv_pop_male_le112,
(CASE
	 when sum_pop_male_le119 <> 0
	 then ((sqrt(sum_varpop_male_le119))/sum_pop_male_le119)
	 else 0
	 end) cv_pop_male_le119,
(CASE
	 when sum_pop_male_113to124 <> 0
	 then ((sqrt(sum_varpop_male_113to124))/sum_pop_male_113to124)
	 else 0
	 end) cv_pop_male_113to124,
(CASE
	 when sum_pop_male_ge113 <> 0
	 then ((sqrt(sum_varpop_male_ge113))/sum_pop_male_ge113)
	 else 0
	 end) cv_pop_male_ge113,	 
(CASE
	 when sum_pop_male_ge120 <> 0
	 then ((sqrt(sum_varpop_male_ge120))/sum_pop_male_ge120)
	 else 0
	 end) cv_pop_male_ge120,	 
(CASE
	 when sum_pop_male_ge110 <> 0
	 then ((sqrt(sum_varpop_male_ge110))/sum_pop_male_ge110)
	 else 0
	 end) cv_pop_male_ge110,	 	 
(CASE
	 when sum_pop_male_ge125 <> 0
	 then ((sqrt(sum_varpop_male_ge125))/sum_pop_male_ge125)
	 else 0
	 end) cv_pop_male_ge125,
(CASE
	 when sum_pop_male_total <> 0
	 then ((sqrt(sum_varpop_male_total))/sum_pop_male_total)
	 else 0
	 end) cv_pop_male_total,
(CASE
	 when sum_pop_female_immature <> 0
	 then ((sqrt(sum_varpop_female_immature))/sum_pop_female_immature)
	 else 0
	 end) cv_pop_female_immature,	     
(CASE
	 when sum_pop_female_mature <> 0
	 then ((sqrt(sum_varpop_female_mature))/sum_pop_female_mature)
	 else 0
	 end) cv_pop_female_mature,	  
(CASE
	 when sum_pop_female_total <> 0
	 then ((sqrt(sum_varpop_female_total))/sum_pop_female_total)
	 else 0
	 end) cv_pop_female_total,
(CASE
	 when sum_pop_unsexed_total <> 0
	 then ((sqrt(sum_varpop_unsexed_total))/sum_pop_unsexed_total)
	 else 0
	 end) cv_pop_unsexed_total,
(CASE
	 when sum_pop_gtotal <> 0
	 then ((sqrt(sum_varpop_gtotal))/sum_pop_gtotal)
	 else 0
	 end) cv_pop_gtotal	  	 	 	 	  
from e166cb_varpop_sizegroup_sum a, e166cb_pop_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;	 	 	 

drop table e166cb_bio_sizegroup_cv;

create table e166cb_bio_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_bio_male_le94 <> 0
	 then ((sqrt(sum_varbio_male_le94))/sum_bio_male_le94)
	 else 0
	 end) cv_bio_male_le94,
(CASE
	 when sum_bio_male_le109 <> 0
	 then ((sqrt(sum_varbio_male_le109))/sum_bio_male_le109)
	 else 0
	 end) cv_bio_male_le109,
(CASE
	 when sum_bio_male_le112 <> 0
	 then ((sqrt(sum_varbio_male_le112))/sum_bio_male_le112)
	 else 0
	 end) cv_bio_male_le112,
(CASE
	 when sum_bio_male_le119 <> 0
	 then ((sqrt(sum_varbio_male_le119))/sum_bio_male_le119)
	 else 0
	 end) cv_bio_male_le119,
(CASE
	 when sum_bio_male_113to124 <> 0
	 then ((sqrt(sum_varbio_male_113to124))/sum_bio_male_113to124)
	 else 0
	 end) cv_bio_male_113to124,
(CASE
	 when sum_bio_male_ge113 <> 0
	 then ((sqrt(sum_varbio_male_ge113))/sum_bio_male_ge113)
	 else 0
	 end) cv_bio_male_ge113,	 
(CASE
	 when sum_bio_male_ge120 <> 0
	 then ((sqrt(sum_varbio_male_ge120))/sum_bio_male_ge120)
	 else 0
	 end) cv_bio_male_ge120,	 
(CASE
	 when sum_bio_male_ge110 <> 0
	 then ((sqrt(sum_varbio_male_ge110))/sum_bio_male_ge110)
	 else 0
	 end) cv_bio_male_ge110,	 	 
(CASE
	 when sum_bio_male_ge125 <> 0
	 then ((sqrt(sum_varbio_male_ge125))/sum_bio_male_ge125)
	 else 0
	 end) cv_bio_male_ge125,
(CASE
	 when sum_bio_male_total <> 0
	 then ((sqrt(sum_varbio_male_total))/sum_bio_male_total)
	 else 0
	 end) cv_bio_male_total,
(CASE
	 when sum_bio_female_immature <> 0
	 then ((sqrt(sum_varbio_female_immature))/sum_bio_female_immature)
	 else 0
	 end) cv_bio_female_immature,	     
(CASE
	 when sum_bio_female_mature <> 0
	 then ((sqrt(sum_varbio_female_mature))/sum_bio_female_mature)
	 else 0
	 end) cv_bio_female_mature,	  
(CASE
	 when sum_bio_female_total <> 0
	 then ((sqrt(sum_varbio_female_total))/sum_bio_female_total)
	 else 0
	 end) cv_bio_female_total,
(CASE
	 when sum_bio_gtotal <> 0
	 then ((sqrt(sum_varbio_gtotal))/sum_bio_gtotal)
	 else 0
	 end) cv_bio_gtotal	 
from e166cb_varbio_sizegroup_sum a, e166cb_bio_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;


-- CI calcs

create or replace view e166cb_sizegroup_stderr_pop as
select distinct survey_year,
(sqrt(sum_varpop_male_le94)) stderr_pop_male_le94,
(sqrt(sum_varpop_male_le109)) stderr_pop_male_le109,
(sqrt(sum_varpop_male_le112)) stderr_pop_male_le112,
(sqrt(sum_varpop_male_le119)) stderr_pop_male_le119,
(sqrt(sum_varpop_male_113to124)) stderr_pop_male_113to124,
(sqrt(sum_varpop_male_ge113)) stderr_pop_male_ge113,
(sqrt(sum_varpop_male_ge120)) stderr_pop_male_ge120,
(sqrt(sum_varpop_male_ge110)) stderr_pop_male_ge110,
(sqrt(sum_varpop_male_ge125)) stderr_pop_male_ge125,
(sqrt(sum_varpop_male_total)) stderr_pop_male_total,
(sqrt(sum_varpop_female_immature)) stderr_pop_female_immature,
(sqrt(sum_varpop_female_mature)) stderr_pop_female_mature,
(sqrt(sum_varpop_female_total)) stderr_pop_female_total,
(sqrt(sum_varpop_unsexed_total)) stderr_pop_unsexed_total,
(sqrt(sum_varpop_gtotal)) stderr_pop_gtotal
from e166cb_varpop_sizegroup_sum;

create or replace view e166cb_sizegroup_stderr_bio as
select distinct survey_year,
(sqrt(sum_varbio_male_le94)) stderr_bio_male_le94,
(sqrt(sum_varbio_male_le109)) stderr_bio_male_le109,
(sqrt(sum_varbio_male_le112)) stderr_bio_male_le112,
(sqrt(sum_varbio_male_le119)) stderr_bio_male_le119,
(sqrt(sum_varbio_male_113to124)) stderr_bio_male_113to124,
(sqrt(sum_varbio_male_ge113)) stderr_bio_male_ge113,
(sqrt(sum_varbio_male_ge120)) stderr_bio_male_ge120,
(sqrt(sum_varbio_male_ge110)) stderr_bio_male_ge110,
(sqrt(sum_varbio_male_ge125)) stderr_bio_male_ge125,
(sqrt(sum_varbio_male_total)) stderr_bio_male_total,
(sqrt(sum_varbio_female_immature)) stderr_bio_female_immature,
(sqrt(sum_varbio_female_mature)) stderr_bio_female_mature,
(sqrt(sum_varbio_female_total)) stderr_bio_female_total,
(sqrt(sum_varbio_gtotal)) stderr_bio_gtotal
from e166cb_varbio_sizegroup_sum;

drop table e166cb_sizegroup_ci_pop;

create table e166cb_sizegroup_ci_pop as
select distinct survey_year,
(1.96 * stderr_pop_male_le94) ci_pop_male_le94,
(1.96 * stderr_pop_male_le109) ci_pop_male_le109,
(1.96 * stderr_pop_male_le112) ci_pop_male_le112,
(1.96 * stderr_pop_male_le119) ci_pop_male_le119,
(1.96 * stderr_pop_male_113to124) ci_pop_male_113to124,
(1.96 * stderr_pop_male_ge113) ci_pop_male_ge113,
(1.96 * stderr_pop_male_ge120) ci_pop_male_ge120,
(1.96 * stderr_pop_male_ge110) ci_pop_male_ge110,
(1.96 * stderr_pop_male_ge125) ci_pop_male_ge125,
(1.96 * stderr_pop_male_total) ci_pop_male_total,
(1.96 * stderr_pop_female_immature) ci_pop_female_immature,
(1.96 * stderr_pop_female_mature) ci_pop_female_mature,
(1.96 * stderr_pop_female_total) ci_pop_female_total,
(1.96 * stderr_pop_unsexed_total) ci_pop_unsexed_total,
(1.96 * stderr_pop_gtotal) ci_pop_gtotal
from e166cb_sizegroup_stderr_pop;

drop table e166cb_sizegroup_ci_bio;

create table e166cb_sizegroup_ci_bio as
select distinct survey_year,
((1.96 * stderr_bio_male_le94)) ci_bio_male_le94,
((1.96 * stderr_bio_male_le109)) ci_bio_male_le109,
((1.96 * stderr_bio_male_le112)) ci_bio_male_le112,
((1.96 * stderr_bio_male_le119)) ci_bio_male_le119,
((1.96 * stderr_bio_male_113to124)) ci_bio_male_113to124,
((1.96 * stderr_bio_male_ge113)) ci_bio_male_ge113,
((1.96 * stderr_bio_male_ge120)) ci_bio_male_ge120,
((1.96 * stderr_bio_male_ge110)) ci_bio_male_ge110,
((1.96 * stderr_bio_male_ge125)) ci_bio_male_ge125,
((1.96 * stderr_bio_male_total)) ci_bio_male_total,
((1.96 * stderr_bio_female_immature)) ci_bio_female_immature,
((1.96 * stderr_bio_female_mature)) ci_bio_female_mature,
((1.96 * stderr_bio_female_total)) ci_bio_female_total,
((1.96 * stderr_bio_gtotal)) ci_bio_gtotal
from e166cb_sizegroup_stderr_bio;


--  west of 166W

drop table w166cb_pop_sizegroup;

create table w166cb_pop_sizegroup as
select survey_year,
sum(pop_male_le94) sum_pop_male_le94,
sum(pop_male_le109) sum_pop_male_le109,
sum(pop_male_le102) sum_pop_male_le102,
sum(pop_male_le119) sum_pop_male_le119,
sum(pop_male_103to124) sum_pop_male_103to124,
sum(pop_male_ge103) sum_pop_male_ge103,
sum(pop_male_ge120) sum_pop_male_ge120,
sum(pop_male_ge125) sum_pop_male_ge125,
sum(pop_male_ge110) sum_pop_male_ge110,
sum(pop_male_ge138) sum_pop_male_ge138,
sum(pop_male_total) sum_pop_male_total,
sum(pop_female_immature) sum_pop_female_immature,
sum(pop_female_mature) sum_pop_female_mature,
sum(pop_female_total) sum_pop_female_total,
sum(pop_unsexed_total) sum_pop_unsexed_total,
sum(pop_gtotal) sum_pop_gtotal
from cb_popbystratum_sizegroup
where district in ('Pribilof MTCA','St. Matthew MTCA','West 166')
group by survey_year
order by survey_year;

drop table w166cb_bio_sizegroup;

create table w166cb_bio_sizegroup as
select survey_year,
sum(bio_male_le94) sum_bio_male_le94,
sum(bio_male_le109) sum_bio_male_le109,
sum(bio_male_le102) sum_bio_male_le102,
sum(bio_male_le119) sum_bio_male_le119,
sum(bio_male_103to124) sum_bio_male_103to124,
sum(bio_male_ge103) sum_bio_male_ge103,
sum(bio_male_ge120) sum_bio_male_ge120,
sum(bio_male_ge125) sum_bio_male_ge125,
sum(bio_male_ge110) sum_bio_male_ge110,
sum(bio_male_ge138) sum_bio_male_ge138,
sum(bio_male_total) sum_bio_male_total,
sum(bio_female_immature) sum_bio_female_immature,
sum(bio_female_mature) sum_bio_female_mature,
sum(bio_female_total) sum_bio_female_total,
sum(bio_gtotal) sum_bio_gtotal
from cb_biobystratum_sizegroup
where district in ('Pribilof MTCA','St. Matthew MTCA','West 166')
group by survey_year
order by survey_year;


drop table w166cb_varpop_sizegroup_sum;

create table w166cb_varpop_sizegroup_sum as
select distinct survey_year,
sum(varpop_male_le94) sum_varpop_male_le94,
sum(varpop_male_le109) sum_varpop_male_le109,
sum(varpop_male_le102) sum_varpop_male_le102,
sum(varpop_male_le119) sum_varpop_male_le119,
sum(varpop_male_103to124) sum_varpop_male_103to124,
sum(varpop_male_ge103) sum_varpop_male_ge103,
sum(varpop_male_ge120) sum_varpop_male_ge120,
sum(varpop_male_ge125) sum_varpop_male_ge125,
sum(varpop_male_ge110) sum_varpop_male_ge110,
sum(varpop_male_ge138) sum_varpop_male_ge138,
sum(varpop_male_total) sum_varpop_male_total,
sum(varpop_female_immature) sum_varpop_female_immature,
sum(varpop_female_mature) sum_varpop_female_mature,
sum(varpop_female_total) sum_varpop_female_total,
sum(varpop_unsexed_total) sum_varpop_unsexed_total,
sum(varpop_gtotal) sum_varpop_gtotal
from cb_variancepop_sizegroup
where district in ('Pribilof MTCA','St. Matthew MTCA','West 166')
group by survey_year
order by survey_year;

drop table w166cb_varbio_sizegroup_sum;

create table w166cb_varbio_sizegroup_sum as
select distinct survey_year,
sum(varbio_male_le94) sum_varbio_male_le94,
sum(varbio_male_le109) sum_varbio_male_le109,
sum(varbio_male_le102) sum_varbio_male_le102,
sum(varbio_male_le119) sum_varbio_male_le119,
sum(varbio_male_103to124) sum_varbio_male_103to124,
sum(varbio_male_ge103) sum_varbio_male_ge103,
sum(varbio_male_ge120) sum_varbio_male_ge120,
sum(varbio_male_ge125) sum_varbio_male_ge125,
sum(varbio_male_ge110) sum_varbio_male_ge110,
sum(varbio_male_ge138) sum_varbio_male_ge138,
sum(varbio_male_total) sum_varbio_male_total,
sum(varbio_female_immature) sum_varbio_female_immature,
sum(varbio_female_mature) sum_varbio_female_mature,
sum(varbio_female_total) sum_varbio_female_total,
sum(varbio_gtotal) sum_varbio_gtotal
from cb_variancebio_sizegroup
where district in ('Pribilof MTCA','St. Matthew MTCA','West 166')
group by survey_year
order by survey_year;

drop table w166cb_pop_sizegroup_cv;

create table w166cb_pop_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_pop_male_le94 <> 0
	 then ((sqrt(sum_varpop_male_le94))/sum_pop_male_le94)
	 else 0
	 end) cv_pop_male_le94,
(CASE
	 when sum_pop_male_le109 <> 0
	 then ((sqrt(sum_varpop_male_le109))/sum_pop_male_le109)
	 else 0
	 end) cv_pop_male_le109,
(CASE
	 when sum_pop_male_le102 <> 0
	 then ((sqrt(sum_varpop_male_le102))/sum_pop_male_le102)
	 else 0
	 end) cv_pop_male_le102,
(CASE
	 when sum_pop_male_le119 <> 0
	 then ((sqrt(sum_varpop_male_le119))/sum_pop_male_le119)
	 else 0
	 end) cv_pop_male_le119,
(CASE
	 when sum_pop_male_103to124 <> 0
	 then ((sqrt(sum_varpop_male_103to124))/sum_pop_male_103to124)
	 else 0
	 end) cv_pop_male_103to124,
(CASE
	 when sum_pop_male_ge103 <> 0
	 then ((sqrt(sum_varpop_male_ge103))/sum_pop_male_ge103)
	 else 0
	 end) cv_pop_male_ge103,	 
(CASE
	 when sum_pop_male_ge120 <> 0
	 then ((sqrt(sum_varpop_male_ge120))/sum_pop_male_ge120)
	 else 0
	 end) cv_pop_male_ge120,	 
(CASE
	 when sum_pop_male_ge125 <> 0
	 then ((sqrt(sum_varpop_male_ge125))/sum_pop_male_ge125)
	 else 0
	 end) cv_pop_male_ge125,	 	    
(CASE
	 when sum_pop_male_ge110 <> 0
	 then ((sqrt(sum_varpop_male_ge110))/sum_pop_male_ge110)
	 else 0
	 end) cv_pop_male_ge110,	 	 
(CASE
	 when sum_pop_male_ge138 <> 0
	 then ((sqrt(sum_varpop_male_ge138))/sum_pop_male_ge138)
	 else 0
	 end) cv_pop_male_ge138,
(CASE
	 when sum_pop_male_total <> 0
	 then ((sqrt(sum_varpop_male_total))/sum_pop_male_total)
	 else 0
	 end) cv_pop_male_total,
(CASE
	 when sum_pop_female_immature <> 0
	 then ((sqrt(sum_varpop_female_immature))/sum_pop_female_immature)
	 else 0
	 end) cv_pop_female_immature,	 
(CASE
	 when sum_pop_female_mature <> 0
	 then ((sqrt(sum_varpop_female_mature))/sum_pop_female_mature)
	 else 0
	 end) cv_pop_female_mature,	 
(CASE
	 when sum_pop_female_total <> 0
	 then ((sqrt(sum_varpop_female_total))/sum_pop_female_total)
	 else 0
	 end) cv_pop_female_total,
(CASE
	 when sum_pop_unsexed_total <> 0
	 then ((sqrt(sum_varpop_unsexed_total))/sum_pop_unsexed_total)
	 else 0
	 end) cv_pop_unsexed_total,
(CASE
	 when sum_pop_gtotal <> 0
	 then ((sqrt(sum_varpop_gtotal))/sum_pop_gtotal)
	 else 0
	 end) cv_pop_gtotal	  	 	 	 	  
from w166cb_varpop_sizegroup_sum a, w166cb_pop_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;	 	 	 

drop table w166cb_bio_sizegroup_cv;

create table w166cb_bio_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_bio_male_le94 <> 0
	 then ((sqrt(sum_varbio_male_le94))/sum_bio_male_le94)
	 else 0
	 end) cv_bio_male_le94,
(CASE
	 when sum_bio_male_le109 <> 0
	 then ((sqrt(sum_varbio_male_le109))/sum_bio_male_le109)
	 else 0
	 end) cv_bio_male_le109,
(CASE
	 when sum_bio_male_le102 <> 0
	 then ((sqrt(sum_varbio_male_le102))/sum_bio_male_le102)
	 else 0
	 end) cv_bio_male_le102,
(CASE
	 when sum_bio_male_le119 <> 0
	 then ((sqrt(sum_varbio_male_le119))/sum_bio_male_le119)
	 else 0
	 end) cv_bio_male_le119,
(CASE
	 when sum_bio_male_103to124 <> 0
	 then ((sqrt(sum_varbio_male_103to124))/sum_bio_male_103to124)
	 else 0
	 end) cv_bio_male_103to124,
(CASE
	 when sum_bio_male_ge103 <> 0
	 then ((sqrt(sum_varbio_male_ge103))/sum_bio_male_ge103)
	 else 0
	 end) cv_bio_male_ge103,	 
(CASE
	 when sum_bio_male_ge120 <> 0
	 then ((sqrt(sum_varbio_male_ge120))/sum_bio_male_ge120)
	 else 0
	 end) cv_bio_male_ge120,	 
(CASE
	 when sum_bio_male_ge125 <> 0
	 then ((sqrt(sum_varbio_male_ge125))/sum_bio_male_ge125)
	 else 0
	 end) cv_bio_male_ge125,	 	   
(CASE
	 when sum_bio_male_ge110 <> 0
	 then ((sqrt(sum_varbio_male_ge110))/sum_bio_male_ge110)
	 else 0
	 end) cv_bio_male_ge110,	 	 
(CASE
	 when sum_bio_male_ge138 <> 0
	 then ((sqrt(sum_varbio_male_ge138))/sum_bio_male_ge138)
	 else 0
	 end) cv_bio_male_ge138,
(CASE
	 when sum_bio_male_total <> 0
	 then ((sqrt(sum_varbio_male_total))/sum_bio_male_total)
	 else 0
	 end) cv_bio_male_total,
(CASE
	 when sum_bio_female_immature <> 0
	 then ((sqrt(sum_varbio_female_immature))/sum_bio_female_immature)
	 else 0
	 end) cv_bio_female_immature,	 
(CASE
	 when sum_bio_female_mature <> 0
	 then ((sqrt(sum_varbio_female_mature))/sum_bio_female_mature)
	 else 0
	 end) cv_bio_female_mature,	 
(CASE
	 when sum_bio_female_total <> 0
	 then ((sqrt(sum_varbio_female_total))/sum_bio_female_total)
	 else 0
	 end) cv_bio_female_total,
(CASE
	 when sum_bio_gtotal <> 0
	 then ((sqrt(sum_varbio_gtotal))/sum_bio_gtotal)
	 else 0
	 end) cv_bio_gtotal
from w166cb_varbio_sizegroup_sum a, w166cb_bio_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;


-- CI calcs

create or replace view w166cb_sizegroup_stderr_pop as
select distinct survey_year,
(sqrt(sum_varpop_male_le94)) stderr_pop_male_le94,
(sqrt(sum_varpop_male_le109)) stderr_pop_male_le109,
(sqrt(sum_varpop_male_le102)) stderr_pop_male_le102,
(sqrt(sum_varpop_male_le119)) stderr_pop_male_le119,
(sqrt(sum_varpop_male_103to124)) stderr_pop_male_103to124,
(sqrt(sum_varpop_male_ge103)) stderr_pop_male_ge103,
(sqrt(sum_varpop_male_ge120)) stderr_pop_male_ge120,
(sqrt(sum_varpop_male_ge125)) stderr_pop_male_ge125,
(sqrt(sum_varpop_male_ge110)) stderr_pop_male_ge110,
(sqrt(sum_varpop_male_ge138)) stderr_pop_male_ge138,
(sqrt(sum_varpop_male_total)) stderr_pop_male_total,
(sqrt(sum_varpop_female_immature)) stderr_pop_female_immature,
(sqrt(sum_varpop_female_mature)) stderr_pop_female_mature,
(sqrt(sum_varpop_female_total)) stderr_pop_female_total,
(sqrt(sum_varpop_unsexed_total)) stderr_pop_unsexed_total,
(sqrt(sum_varpop_gtotal)) stderr_pop_gtotal
from w166cb_varpop_sizegroup_sum;

create or replace view w166cb_sizegroup_stderr_bio as
select distinct survey_year,
(sqrt(sum_varbio_male_le94)) stderr_bio_male_le94,
(sqrt(sum_varbio_male_le109)) stderr_bio_male_le109,
(sqrt(sum_varbio_male_le102)) stderr_bio_male_le102,
(sqrt(sum_varbio_male_le119)) stderr_bio_male_le119,
(sqrt(sum_varbio_male_103to124)) stderr_bio_male_103to124,
(sqrt(sum_varbio_male_ge103)) stderr_bio_male_ge103,
(sqrt(sum_varbio_male_ge120)) stderr_bio_male_ge120,
(sqrt(sum_varbio_male_ge125)) stderr_bio_male_ge125,
(sqrt(sum_varbio_male_ge110)) stderr_bio_male_ge110,
(sqrt(sum_varbio_male_ge138)) stderr_bio_male_ge138,
(sqrt(sum_varbio_male_total)) stderr_bio_male_total,
(sqrt(sum_varbio_female_immature)) stderr_bio_female_immature,
(sqrt(sum_varbio_female_mature)) stderr_bio_female_mature,
(sqrt(sum_varbio_female_total)) stderr_bio_female_total,
(sqrt(sum_varbio_gtotal)) stderr_bio_gtotal
from w166cb_varbio_sizegroup_sum;

drop table w166cb_sizegroup_ci_pop;

create table w166cb_sizegroup_ci_pop as
select distinct survey_year,
(1.96 * stderr_pop_male_le94) ci_pop_male_le94,
(1.96 * stderr_pop_male_le109) ci_pop_male_le109,
(1.96 * stderr_pop_male_le102) ci_pop_male_le102,
(1.96 * stderr_pop_male_le119) ci_pop_male_le119,
(1.96 * stderr_pop_male_103to124) ci_pop_male_103to124,
(1.96 * stderr_pop_male_ge103) ci_pop_male_ge103,
(1.96 * stderr_pop_male_ge120) ci_pop_male_ge120,
(1.96 * stderr_pop_male_ge125) ci_pop_male_ge125,
(1.96 * stderr_pop_male_ge110) ci_pop_male_ge110,
(1.96 * stderr_pop_male_ge138) ci_pop_male_ge138,
(1.96 * stderr_pop_male_total) ci_pop_male_total,
(1.96 * stderr_pop_female_immature) ci_pop_female_immature,
(1.96 * stderr_pop_female_mature) ci_pop_female_mature,
(1.96 * stderr_pop_female_total) ci_pop_female_total,
(1.96 * stderr_pop_unsexed_total) ci_pop_unsexed_total,
(1.96 * stderr_pop_gtotal) ci_pop_gtotal
from w166cb_sizegroup_stderr_pop;

drop table w166cb_sizegroup_ci_bio;

create table w166cb_sizegroup_ci_bio as
select distinct survey_year,
((1.96 * stderr_bio_male_le94)) ci_bio_male_le94,
((1.96 * stderr_bio_male_le109)) ci_bio_male_le109,
((1.96 * stderr_bio_male_le102)) ci_bio_male_le102,
((1.96 * stderr_bio_male_le119)) ci_bio_male_le119,
((1.96 * stderr_bio_male_103to124)) ci_bio_male_103to124,
((1.96 * stderr_bio_male_ge103)) ci_bio_male_ge103,
((1.96 * stderr_bio_male_ge120)) ci_bio_male_ge120,
((1.96 * stderr_bio_male_ge125)) ci_bio_male_ge125,
((1.96 * stderr_bio_male_ge110)) ci_bio_male_ge110,
((1.96 * stderr_bio_male_ge138)) ci_bio_male_ge138,
((1.96 * stderr_bio_male_total)) ci_bio_male_total,
((1.96 * stderr_bio_female_immature)) ci_bio_female_immature,
((1.96 * stderr_bio_female_mature)) ci_bio_female_mature,
((1.96 * stderr_bio_female_total)) ci_bio_female_total,
((1.96 * stderr_bio_gtotal)) ci_bio_gtotal
from w166cb_sizegroup_stderr_bio;


-- Final output for stocks

-- All Districts Combined

drop table cb_all_bio_matfem_newregress_biascorrected;

create table cb_all_bio_matfem_newregress_biascorrected as
select a.survey_year,
sum_bio_male_le112 biomass_male_le112,cv_bio_male_le112 cv_biomass_male_le112,ci_bio_male_le112 ci_biomass_male_le112,
sum_bio_male_ge113 biomass_male_ge113,cv_bio_male_ge113 cv_biomass_male_ge113,ci_bio_male_ge113 ci_biomass_male_ge113,
sum_bio_male_ge120 biomass_male_ge120,cv_bio_male_ge120 cv_biomass_male_ge120,ci_bio_male_ge120 ci_biomass_male_ge120,
sum_bio_male_ge125 biomass_male_ge125,cv_bio_male_ge125 cv_biomass_male_ge125,ci_bio_male_ge125 ci_biomass_male_ge125,
sum_bio_male_113to124 biomass_male_113to124,cv_bio_male_113to124 cv_biomass_male_113to124,ci_bio_male_113to124 ci_biomass_male_113to124,
sum_bio_male_total biomass_male_total,cv_bio_male_total cv_biomass_male_total,ci_bio_male_total ci_biomass_male_total,
--sum_bio_female_le79 mt_female_immature,cv_bio_female_le79 cv_mt_female_immature,ci_bio_female_le79 ci_mt_female_immature,
--sum_bio_female_ge80 mt_female_mature,cv_bio_female_ge80 cv_mt_female_mature,ci_bio_female_ge80 ci_mt_female_mature,
sum_bio_female_immature biomass_female_immature,cv_bio_female_immature cv_biomass_female_immature,ci_bio_female_immature ci_biomass_female_immature,
--sum_bio_female_le69 mt_female_le69,cv_bio_female_le69 cv_mt_female_le69,ci_bio_female_le69 ci_mt_female_le69,
--sum_bio_female_ge70 mt_female_ge70,cv_bio_female_ge70 cv_mt_female_ge70,ci_bio_female_ge70 ci_mt_female_ge70,
sum_bio_female_mature biomass_female_mature,cv_bio_female_mature cv_biomass_female_mature,ci_bio_female_mature ci_biomass_female_mature,
sum_bio_female_total biomass_female_total,cv_bio_female_total cv_biomass_female_total,ci_bio_female_total ci_biomass_female_total
from cb_bioall_sizegroup a, cb_sizegroup_confidence_bio b,cb_bio_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
--and a.survey_year = 2013
order by a.survey_year;





-- East of 166W

drop table cb_e166_bio_matfem_newmodel_biascorrected;

create table cb_e166_bio_matfem_newmodel_biascorrected as
select a.survey_year,
sum_bio_male_le112 biomass_male_le112,cv_bio_male_le112 cv_biomass_male_le112,ci_bio_male_le112 ci_biomass_male_le112,
sum_bio_male_ge113 biomass_male_ge113,cv_bio_male_ge113 cv_biomass_male_ge113,ci_bio_male_ge113 ci_biomass_male_ge113,
sum_bio_male_ge120 biomass_male_ge120,cv_bio_male_ge120 cv_biomass_male_ge120,ci_bio_male_ge120 ci_biomass_male_ge120,
sum_bio_male_ge125 biomass_male_ge125,cv_bio_male_ge125 cv_biomass_male_ge125,ci_bio_male_ge125 ci_biomass_male_ge125,
sum_bio_male_113to124 biomass_male_113to124,cv_bio_male_113to124 cv_biomass_male_113to124,ci_bio_male_113to124 ci_biomass_male_113to124,
sum_bio_male_total biomass_male_total,cv_bio_male_total cv_biomass_male_total,ci_bio_male_total ci_biomass_male_total,
--sum_bio_female_le79 mt_female_immature,cv_bio_female_le79 cv_mt_female_immature,ci_bio_female_le79 ci_mt_female_immature,
--sum_bio_female_ge80 mt_female_mature,cv_bio_female_ge80 cv_mt_female_mature,ci_bio_female_ge80 ci_mt_female_mature,
sum_bio_female_immature biomass_female_immature,cv_bio_female_immature cv_biomass_female_immature,ci_bio_female_immature ci_biomass_female_immature,
--sum_bio_female_le69 mt_female_le69,cv_bio_female_le69 cv_mt_female_le69,ci_bio_female_le69 ci_mt_female_le69,
--sum_bio_female_ge70 mt_female_ge70,cv_bio_female_ge70 cv_mt_female_ge70,ci_bio_female_ge70 ci_mt_female_ge70,
sum_bio_female_mature biomass_female_mature,cv_bio_female_mature cv_biomass_female_mature,ci_bio_female_mature ci_biomass_female_mature,
sum_bio_female_total biomass_female_total,cv_bio_female_total cv_biomass_female_total,ci_bio_female_total ci_biomass_female_total
--sum_bio_gtotal gtotal_mt,cv_bio_gtotal cv_gtotal_mt,ci_bio_gtotal ci_gtotal_mt
from e166cb_bio_sizegroup a, e166cb_sizegroup_ci_bio b,e166cb_bio_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
--and a.survey_year = 2013
order by a.survey_year;



-- West of 166W

drop table cb_w166_bio_matfem_newmodel_biascorrected;

create table cb_w166_bio_matfem_newmodel_biascorrected as
select a.survey_year,
sum_bio_male_le102 biomass_male_le102,cv_bio_male_le102 cv_biomass_male_le102,ci_bio_male_le102 ci_biomass_male_le102,
sum_bio_male_ge103 biomass_male_ge103,cv_bio_male_ge103 cv_biomass_male_ge103,ci_bio_male_ge103 ci_biomass_male_ge103,
sum_bio_male_ge110 biomass_male_ge110,cv_bio_male_ge110 cv_biomass_male_ge110,ci_bio_male_ge110 ci_biomass_male_ge110,
sum_bio_male_103to124 biomass_male_103to124,cv_bio_male_103to124 cv_biomass_male_103to124,ci_bio_male_103to124 ci_biomass_male_103to124,
sum_bio_male_ge125 biomass_male_ge125,cv_bio_male_ge125 cv_biomass_male_ge125,ci_bio_male_ge125 ci_biomass_male_ge125,
--sum_bio_male_103to124 biomass_male_103to124,cv_bio_male_103to124 cv_biomass_male_103to124,ci_bio_male_103to124 ci_biomass_male_103to124,
sum_bio_male_total biomass_male_total,cv_bio_male_total cv_biomass_male_total,ci_bio_male_total ci_biomass_male_total,
sum_bio_female_immature biomass_female_immature,cv_bio_female_immature cv_biomass_female_immature,ci_bio_female_immature ci_biomass_female_immature,
sum_bio_female_mature biomass_female_mature,cv_bio_female_mature cv_biomass_female_mature,ci_bio_female_mature ci_biomass_female_mature,
--sum_bio_female_le84,cv_bio_female_le84,ci_bio_female_le84,ci_bio_percent_female_le84,
--sum_bio_female_le69 biomass_female_le69,cv_bio_female_le69 cv_biomass_female_le69,ci_bio_female_le69 ci_biomass_female_le69,
--sum_bio_female_ge70 biomass_female_ge70,cv_bio_female_ge70 cv_biomass_female_ge70,ci_bio_female_ge70 ci_biomass_female_ge70,
--sum_bio_female_ge85,cv_bio_female_ge85,ci_bio_female_ge85,ci_bio_percent_female_ge85,
sum_bio_female_total biomass_female_total,cv_bio_female_total cv_biomass_female_total,ci_bio_female_total ci_biomass_female_total
from w166cb_bio_sizegroup a, w166cb_sizegroup_ci_bio b,w166cb_bio_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
--and a.survey_year = 2013
order by a.survey_year;



/*
------------------------------Abundance ----------------------------------------------------------

drop table cb_all_pop_matfem_newregress;

create table cb_all_pop_matfem_newregress as
select a.survey_year,
sum_pop_male_le112 num_male_le112,cv_pop_male_le112 cv_num_male_le112,ci_pop_male_le112 ci_num_male_le112,
sum_pop_male_ge113 num_male_ge113,cv_pop_male_ge113 cv_num_male_ge113,ci_pop_male_ge113 ci_num_male_ge113,
sum_pop_male_ge120 num_male_ge120,cv_pop_male_ge120 cv_num_male_ge120,ci_pop_male_ge120 ci_num_male_ge120,
sum_pop_male_ge125 num_male_ge125,cv_pop_male_ge125 cv_num_male_ge125,ci_pop_male_ge125 ci_num_male_ge125,
sum_pop_male_113to124 num_male_113to124,cv_pop_male_113to124 cv_num_male_113to124,ci_pop_male_113to124 ci_num_male_113to124,
sum_pop_male_total num_male_total,cv_pop_male_total cv_num_male_total,ci_pop_male_total ci_num_male_total,
--sum_pop_female_le79 num_female_immature,cv_pop_female_le79 cv_num_female_immature,ci_pop_female_le79 ci_num_female_immature,
--sum_pop_female_ge80 num_female_mature,cv_pop_female_ge80 cv_num_female_mature,ci_pop_female_ge80 ci_num_female_mature,
sum_pop_female_immature num_female_immature,cv_pop_female_immature cv_num_female_immature,ci_pop_female_immature ci_num_female_immature,
sum_pop_female_mature num_female_mature,cv_pop_female_mature cv_num_female_mature,ci_pop_female_mature ci_num_female_mature,
--sum_pop_female_le69 num_female_le69,cv_pop_female_le69 cv_abundance_female_le69,ci_pop_female_le69 ci_abundance_female_le69,
--sum_pop_female_ge70 abundance_female_ge70,cv_pop_female_ge70 cv_abundance_female_ge70,ci_pop_female_ge70 ci_abundance_female_ge70,
sum_pop_female_total num_female_total,cv_pop_female_total cv_num_female_total,ci_pop_female_total ci_num_female_total
from cb_popall_sizegroup a, cb_sizegroup_confidence_pop b,cb_pop_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
--and a.survey_year = 2013
order by a.survey_year;
drop table cb_w166_pop_matfem_newmodel;


drop table cb_e166_pop_matfem_newmodel;

create table cb_e166_pop_matfem_newmodel as
select a.survey_year,
sum_pop_male_le112 num_male_le112,cv_pop_male_le112 cv_num_male_le112,ci_pop_male_le112 ci_num_male_le112,
sum_pop_male_ge113 num_male_ge113,cv_pop_male_ge113 cv_num_male_ge113,ci_pop_male_ge113 ci_num_male_ge113,
sum_pop_male_ge120 num_male_ge120,cv_pop_male_ge120 cv_num_male_ge120,ci_pop_male_ge120 ci_num_male_ge120,
sum_pop_male_ge125 num_male_ge125,cv_pop_male_ge125 cv_num_male_ge125,ci_pop_male_ge125 ci_num_male_ge125,
sum_pop_male_113to124 num_male_113to124,cv_pop_male_113to124 cv_num_male_113to124,ci_pop_male_113to124 ci_num_male_113to124,
sum_pop_male_total num_male_total,cv_pop_male_total cv_num_male_total,ci_pop_male_total ci_num_male_total,
--sum_pop_female_le79 num_female_immature,cv_pop_female_le79 cv_num_female_immature,ci_pop_female_le79 ci_num_female_immature,
--sum_pop_female_ge80 num_female_mature,cv_pop_female_ge80 cv_num_female_mature,ci_pop_female_ge80 ci_num_female_mature,
sum_pop_female_immature num_female_immature,cv_pop_female_immature cv_num_female_immature,ci_pop_female_immature ci_num_female_immature,
sum_pop_female_mature num_female_mature,cv_pop_female_mature cv_num_female_mature,ci_pop_female_mature ci_num_female_mature,
--sum_pop_female_le69 num_female_le69,cv_pop_female_le69 cv_num_female_le69,ci_pop_female_le69 ci_num_female_le69,
--sum_pop_female_ge70 num_female_ge70,cv_pop_female_ge70 cv_num_female_ge70,ci_pop_female_ge70 ci_num_female_ge70,
sum_pop_female_total num_female_total,cv_pop_female_total cv_num_female_total,ci_pop_female_total ci_num_female_total
--sum_pop_gtotal gtotal,cv_pop_gtotal cv_gtotal,ci_pop_gtotal ci_gtotal
from e166cb_pop_sizegroup a, e166cb_sizegroup_ci_pop b,e166cb_pop_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
--and a.survey_year = 2013
order by a.survey_year;


create table cb_w166_pop_matfem_newmodel as
select a.survey_year,
sum_pop_male_le102 num_male_le102,cv_pop_male_le102 cv_num_male_le102,ci_pop_male_le102 ci_num_male_le102,
sum_pop_male_ge103 num_male_ge103,cv_pop_male_ge103 cv_num_male_ge103,ci_pop_male_ge103 ci_num_male_ge103,
sum_pop_male_ge110 num_male_ge110,cv_pop_male_ge110 cv_num_male_ge110,ci_pop_male_ge110 ci_num_male_ge110,
sum_pop_male_103to124 num_male_103to124,cv_pop_male_103to124 cv_num_male_103to124,ci_pop_male_103to124 ci_num_male_103to124,
sum_pop_male_ge125 num_male_ge125,cv_pop_male_ge125 cv_num_male_ge125,ci_pop_male_ge125 ci_num_male_ge125,
--sum_pop_male_103to124 num_male_103to124,cv_pop_male_103to124 cv_num_male_103to124,ci_pop_male_103to124 ci_num_male_103to124,
sum_pop_male_total num_male_total,cv_pop_male_total cv_num_male_total,ci_pop_male_total ci_num_male_total,
sum_pop_female_immature num_female_immature,cv_pop_female_immature cv_num_female_immature,ci_pop_female_immature ci_num_female_immature,
sum_pop_female_mature num_female_mature,cv_pop_female_mature cv_num_female_mature,ci_pop_female_mature ci_num_female_mature,
--sum_pop_female_le84,cv_pop_female_le84,ci_pop_female_le84,ci_pop_percent_female_le84,
--sum_pop_female_le69 num_female_le69,cv_pop_female_le69 cv_num_female_le69,ci_pop_female_le69 ci_num_female_le69,
--sum_pop_female_ge70 num_female_ge70,cv_pop_female_ge70 cv_num_female_ge70,ci_pop_female_ge70 ci_num_female_ge70,
--sum_pop_female_ge85,cv_pop_female_ge85,ci_pop_female_ge85,ci_pop_percent_female_ge85,
sum_pop_female_total num_female_total,cv_pop_female_total cv_num_female_total,ci_pop_female_total ci_num_female_total
from w166cb_pop_sizegroup a, w166cb_sizegroup_ci_pop b,w166cb_pop_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
--and a.survey_year = 2013
order by a.survey_year;

