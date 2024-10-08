-- This script produces a table of population estimates for opilio Tanner
-- crab from the 1980-2016 EBS trawl surveys.  Population is calculated
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

drop table co_number_size1_male;

create table co_number_size1_male as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,shell_condition,
(trunc(width/1) * 1)size1,
(sum(CASE
		 when species_code = 68580
		 and sex = 1
		 then sampling_factor
		 else 0
		 end)) number_male_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 68580
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

drop table co_number_size1_female;

create table co_number_size1_female as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,clutch_size,
(trunc(width/1) * 1)size1,
(sum(CASE
		 when species_code = 68580
		 and sex = 2
		 then sampling_factor
		 else 0
		 end)) number_female_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 68580
and width <> 999
and c.hauljoin(+) = h.hauljoin
and haul_type <> 17
group by c.hauljoin,
	  	 c.vessel,
		 c.cruise,
		 c.haul,
		 h.gis_station,
		 species_code,
     clutch_size,
		 (trunc(width/1) * 1);


-- unsexed

drop table co_number_size1_unsexed;

create table co_number_size1_unsexed as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,
(trunc(width/1) * 1)size1,
(sum(CASE
		 when species_code = 68580
		 and sex = 3
		 then sampling_factor
		 else 0
		 end)) number_unsexed_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 68580
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



--  This section calculates the weight of the opilio Tanner crab by haul, sex,
--  shell condition and 1-mm size group.  A width-weight regression
--  factor is applied, and multiplied by the number of crab caught in that
--  haul/sex/shellcon/size bin (from above section).  
--  The regression factor does not include unsexed crab, therefore no weights
--  will be calculated for unsexed crab

drop table co_weight_grams_male;

create table co_weight_grams_male as
select hauljoin,vessel,cruise,haul,gis_station,species_code,shell_condition,size1,
(CASE
--    WHEN cruise < 201001
--      THEN ((0.00023 * (power(size1,3.12948))) * number_male_size1) 
    WHEN cruise >= 197501 and shell_condition in (1,2)
      THEN ((0.000237 * (power(size1,3.119509))) * number_male_size1)
    WHEN cruise >= 197501 and shell_condition in (3,4,5,0)
      THEN ((0.000343 * (power(size1,3.051748))) * number_male_size1)
    ELSE 0
    END) wgt_male_size1
from co_number_size1_male 
order by cruise,vessel,haul,gis_station,size1;

drop table co_weight_grams_female;

create table co_weight_grams_female as
select hauljoin,vessel,cruise,haul,gis_station,species_code,clutch_size,size1,
(CASE
--    WHEN cruise < 201001
--      THEN ((0.00253 * (power(size1,2.56427))) * number_female_size1)
    WHEN cruise >= 197501 and clutch_size <= 1
      THEN ((0.001047 * (power(size1,2.708367))) * number_female_size1)
    WHEN cruise >= 197501 and clutch_size > 1
      THEN ((0.001158 * (power(size1,2.708793))) * number_female_size1)  
    ELSE 0
    END) wgt_female_size1
from co_number_size1_female
order by cruise,vessel,haul,gis_station,size1;


-- Using actual female maturity in this run, so select for clutch size here

drop table co_number_size1_matfem;

create table co_number_size1_matfem as
select hauljoin, vessel, cruise, haul, gis_station, species_code, size1,
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
    from co_number_size1_female
    where species_code = 68580
		 --and width <> 999
	     --and hauljoin(+) = h.hauljoin
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code,
				size1;
        
-- And calculate weight of crab by actual maturity        

drop table co_weight_grams_matfem;

create table co_weight_grams_matfem as
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
    from co_weight_grams_female
    where species_code = 68580
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

drop table co_weight_grams_size1;

create table co_weight_grams_size1 
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
insert into co_weight_grams_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
wgt_male_size1,null,null
from co_weight_grams_male;

insert into co_weight_grams_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,wgt_female_size1_immature,wgt_female_size1_mature
from co_weight_grams_matfem;

-- convert to metric tons

drop table co_weight_mt_size1;

create table co_weight_mt_size1 as
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
(wgt_male_size1 * 0.000001) mt_male_size1,
(wgt_female_size1_immature * 0.000001) mt_female_size1_immature,
(wgt_female_size1_mature * 0.000001) mt_female_size1_mature
from co_weight_grams_size1
order by cruise,vessel,haul,gis_station,size1;

-- Combine the male, female, and unsexed by number tables

drop table co_number_size1;

create table co_number_size1 
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
insert into co_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
number_male_size1,null,null,null
from co_number_size1_male;

insert into co_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,number_female_size1_immature,number_female_size1_mature,null
from co_number_size1_matfem;

insert into co_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,null,null,number_unsexed_size1
from co_number_size1_unsexed;


-- This section sums the opilio Tanner crab catch records by haul, sex,
-- shell condition, and 1-mm size group.  

drop table co_number_sizegroup;

create table co_number_sizegroup as
select hauljoin, vessel, cruise, haul, gis_station, species_code, 
	   
	   (sum(CASE
	   			WHEN  size1 between 0 and 77.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le77,
				
	   (sum(CASE
	   			WHEN  size1 between 0 and 94.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le94,	  
	   
	   (sum(CASE
	   			WHEN  size1 between 95.0 and 101.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_95to101,	
		
	  (sum(CASE
	   			WHEN  size1 between 25.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge25,	
        
     (sum(CASE
	   			WHEN  size1 between 78.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge78,	
		
	   (sum(CASE
	   			WHEN  size1 between 95.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge95,							
	   
	   (sum(CASE
	   			WHEN  size1 between 102.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge102,
	   
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
	   from co_number_size1
         where species_code = 68580
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code;
				

drop table co_weight_mt_sizegroup;

create table co_weight_mt_sizegroup as
select hauljoin, vessel, cruise, haul, gis_station, species_code, 
	   (sum(CASE
	   			WHEN  size1 between 0 and 77.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le77,
	   (sum(CASE
	   			WHEN  size1 between 0 and 94.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le94,	  
	   (sum(CASE
	   			WHEN  size1 between 95.0 and 101
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_95to101,	
	   (sum(CASE
	   			WHEN  size1 between 25.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge25,
     (sum(CASE
	   			WHEN  size1 between 78.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge78,
	   (sum(CASE
	   			WHEN  size1 between 95.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge95,
	   (sum(CASE
	   			WHEN  size1 between 102.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge102,
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_total,		
	   sum(mt_female_size1_immature) mt_female_immature,
     sum(mt_female_size1_mature) mt_female_mature,
     (sum(mt_female_size1_immature)+ sum(mt_female_size1_mature)) mt_female_total
	   from co_weight_mt_size1
         where species_code = 68580
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code;
				
				

-- This section combines the haul and catch data, including
-- those haul/size groups where there was no catch.				

drop table co_num_sizegroup_union;

create table co_num_sizegroup_union as
select h.hauljoin,h.vessel,h.cruise,h.haul,h.gis_station,h.survey_year,
nvl(species_code,68580)species_code,
nvl(number_male_le77,0) number_male_le77,
nvl(number_male_le94,0) number_male_le94,
nvl(number_male_95to101,0) number_male_95to101,
nvl(number_male_ge25,0) number_male_ge25,
nvl(number_male_ge78,0) number_male_ge78,
nvl(number_male_ge95,0) number_male_ge95,
nvl(number_male_ge102,0) number_male_ge102,
nvl(number_male_total,0) number_male_total,
nvl(number_female_immature,0) number_female_immature,
nvl(number_female_mature,0) number_female_mature,
nvl(number_female_total,0) number_female_total,
nvl(number_unsexed_total,0) number_unsexed_total
from haul_newtimeseries_noretow h full outer join co_number_sizegroup c
on h.hauljoin = c.hauljoin;
--where haul_type <> 17;

--  Similarly, by weight.

drop table co_wgt_sizegroup_union;

create table co_wgt_sizegroup_union as
select h.hauljoin,h.vessel,h.cruise,h.haul,h.gis_station,h.survey_year,
nvl(species_code,68580)species_code,
nvl(mt_male_le77,0) mt_male_le77,
nvl(mt_male_le94,0) mt_male_le94,
nvl(mt_male_95to101,0) mt_male_95to101,
nvl(mt_male_ge25,0) mt_male_ge25,
nvl(mt_male_ge78,0) mt_male_ge78,
nvl(mt_male_ge95,0) mt_male_ge95,
nvl(mt_male_ge102,0) mt_male_ge102,
nvl(mt_male_total,0) mt_male_total,
nvl(mt_female_immature,0) mt_female_immature,
nvl(mt_female_mature,0) mt_female_mature,
nvl(mt_female_total,0) mt_female_total
from haul_newtimeseries_noretow h full outer join co_weight_mt_sizegroup c
on h.hauljoin = c.hauljoin;
--where haul_type <> 17;


-- This section calculates cpue for each haul.
-- If a station contains multiple tows, cpue
-- is calculated for each of the tows, not averaged for the station.
-- A value, even if 0 for no catch, is output for every size group,
-- every haul.  CPUE is calculated as number of crabs per square
-- nautical mile towed; area swept is the distance fished multiplied
-- by the actual (measured) net width.

drop table co_cpuenum_sizegroup;

create table co_cpuenum_sizegroup as
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,
c.gis_station,c.survey_year,c.species_code,h.gear_depth,h.surface_temperature,h.gear_temperature,
(number_male_le77 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le77,
(number_male_le94 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le94,
(number_male_95to101 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_95to101,
(number_male_ge25 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge25,
(number_male_ge78 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge78,
(number_male_ge95 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge95,
(number_male_ge102 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge102,
(number_male_total / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_total,
(number_female_immature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_immature,
(number_female_mature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_mature,
(number_female_total / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_total,
(number_unsexed_total / (((net_width/1000) * distance_fished) * 0.29155335)) unsexed_cpuenum_total
from co_num_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin
and haul_type <> 17;

-- This section calculates cpue by weight for each haul.
-- If a station contains multiple tows, cpue is calculated 
-- for each of the tows, not averaged for the station.
-- A value, even if 0 for no catch, is output for every size group,
-- every haul.  CPUE is calculated as weight of crabs (already converted to metric tons) 
-- per square nautical mile towed; area swept is the distance fished multiplied
-- by the actual (measured) net width.

drop table co_cpuewgt_sizegroup;

create table co_cpuewgt_sizegroup as
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,
c.gis_station,c.survey_year,c.species_code,h.gear_depth,h.surface_temperature,h.gear_temperature,
(mt_male_le77 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le77,
(mt_male_le94 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le94,
(mt_male_95to101 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_95to101,
(mt_male_ge25 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge25,
(mt_male_ge78 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge78,
(mt_male_ge95 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge95,
(mt_male_ge102 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge102,
(mt_male_total / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_total,
(mt_female_immature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_immature,
(mt_female_mature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_mature,
(mt_female_total / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_total
from co_wgt_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin
and haul_type <> 17;



drop table co_male_cpuenum_weight_temp;

create table co_male_cpuenum_weight_temp as 
select survey_year,cruise,
sum(gear_temperature * male_cpuenum_total)/sum(male_cpuenum_total) male_weighted_gear_temp
from co_cpuenum_sizegroup
group by survey_year,cruise;

drop table co_female_cpuenum_weight_temp;

create table co_female_cpuenum_weight_temp as 
select survey_year,cruise,
sum(gear_temperature * female_cpuenum_immature)/sum(female_cpuenum_immature) immature_female_weighted_gear_temp,
sum(gear_temperature * female_cpuenum_mature)/sum(female_cpuenum_mature) mature_female_weighted_gear_temp,
sum(gear_temperature * female_cpuenum_total)/sum(female_cpuenum_total) total_female_weighted_gear_temp
from co_cpuenum_sizegroup
group by survey_year,cruise;


