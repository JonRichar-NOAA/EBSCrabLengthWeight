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
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,shell_condition,clutch_size,
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
         shell_condition,
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
select hauljoin,vessel,a.cruise,haul,gis_station,species_code,shell_condition,size1,
(CASE
--    WHEN cruise < 201001
--      THEN ((0.00023 * (power(size1,3.12948))) * number_male_size1) 
    WHEN a.cruise >= 197501 and shell_condition in (1,2)
      THEN ((p.log10_a_ns * (power(size1,b_ns))) * number_male_size1)
    WHEN a.cruise >= 197501 and shell_condition in (3,4,5,0)
      THEN ((p.log10_a_os * (power(size1,b_os))) * number_male_size1)
    ELSE 0
    END) wgt_male_size1
from co_number_size1_male a, co_male_sw_temp_params p
where a.cruise = p.cruise
order by cruise,vessel,haul,gis_station,size1;

drop table co_weight_grams_female;

create table co_weight_grams_female as
select hauljoin,vessel,a.cruise,haul,gis_station,species_code,shell_condition,clutch_size,size1,
(CASE
--    WHEN cruise < 201001
--      THEN ((0.00253 * (power(size1,2.56427))) * number_female_size1)
    WHEN a.cruise >= 197501 and clutch_size <= 1
      THEN ((0.000778 * (power(size1,2.780558))) * number_female_size1)
    WHEN a.cruise >= 197501 and clutch_size > 1 and shell_condition in (1,2)
      THEN ((p.log10_a_ns  * (power(size1,p.b_ns))) * number_female_size1)
    WHEN a.cruise >= 197501 and clutch_size > 1 and shell_condition in (3,4,5,0)
      THEN ((p.log10_a_os  * (power(size1,p.b_os))) * number_female_size1)
    ELSE 0
    END) wgt_female_size1
from co_number_size1_female a, co_matfemale_sw_temp_params p
where a.cruise = p.cruise
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
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude latitude,mid_longitude longitude,
c.gis_station,c.survey_year,c.species_code,
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
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude latitude,mid_longitude longitude,
c.gis_station,c.survey_year,c.species_code,
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


drop table co_meancpuenum_sizegroup;

create table co_meancpuenum_sizegroup as
select c.survey_year,district,
AVG (male_cpuenum_le77) meancpuenum_male_le77,
AVG (male_cpuenum_le94) meancpuenum_male_le94,
AVG (male_cpuenum_95to101) meancpuenum_male_95to101,
AVG (male_cpuenum_ge25) meancpuenum_male_ge25,
AVG (male_cpuenum_ge78) meancpuenum_male_ge78,
AVG (male_cpuenum_ge95) meancpuenum_male_ge95,
AVG (male_cpuenum_ge102) meancpuenum_male_ge102,
AVG (male_cpuenum_total) meancpuenum_male_total,
AVG (female_cpuenum_immature) meancpuenum_female_immature,
AVG (female_cpuenum_mature) meancpuenum_female_mature,
AVG (female_cpuenum_total) meancpuenum_female_total,
AVG (unsexed_cpuenum_total) meancpuenum_unsexed_total,
AVG (male_cpuenum_total + female_cpuenum_total + unsexed_cpuenum_total) meancpuenum_gtotal
from co_cpuenum_sizegroup c, strata_opilio_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;

drop table co_meancpuewgt_sizegroup;

create table co_meancpuewgt_sizegroup as
select c.survey_year,district,
AVG (male_cpuewgt_le77) meancpuewgt_male_le77,
AVG (male_cpuewgt_le94) meancpuewgt_male_le94,
AVG (male_cpuewgt_95to101) meancpuewgt_male_95to101,
AVG (male_cpuewgt_ge25) meancpuewgt_male_ge25,
AVG (male_cpuewgt_ge78) meancpuewgt_male_ge78,
AVG (male_cpuewgt_ge95) meancpuewgt_male_ge95,
AVG (male_cpuewgt_ge102) meancpuewgt_male_ge102,
AVG (male_cpuewgt_total) meancpuewgt_male_total,
AVG (female_cpuewgt_immature) meancpuewgt_female_immature,
AVG (female_cpuewgt_mature) meancpuewgt_female_mature,
AVG (female_cpuewgt_total) meancpuewgt_female_total,
AVG (male_cpuewgt_total + female_cpuewgt_total) meancpuewgt_gtotal
from co_cpuewgt_sizegroup c, strata_opilio_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;


drop table co_popbystratum_sizegroup;

create table co_popbystratum_sizegroup as
select distinct c.survey_year,stratum,c.district,
 (meancpuenum_male_le77 * total_area) pop_male_le77,
 (meancpuenum_male_le94 * total_area) pop_male_le94,
 (meancpuenum_male_95to101 * total_area) pop_male_95to101,
 (meancpuenum_male_ge25 * total_area) pop_male_ge25,
 (meancpuenum_male_ge78 * total_area) pop_male_ge78,
 (meancpuenum_male_ge95 * total_area) pop_male_ge95,
 (meancpuenum_male_ge102 * total_area) pop_male_ge102,
 (meancpuenum_male_total * total_area) pop_male_total,
 (meancpuenum_female_immature * total_area) pop_female_immature,
 (meancpuenum_female_mature * total_area) pop_female_mature,
 (meancpuenum_female_total * total_area) pop_female_total,
 (meancpuenum_unsexed_total * total_area) pop_unsexed_total,
 (meancpuenum_gtotal * total_area) pop_gtotal
from co_meancpuenum_sizegroup c, strata_opilio_newtimeseries s
where c.district = s.district
and c.survey_year = s.survey_year
order by survey_year,district;

drop table co_biobystratum_sizegroup;

create table co_biobystratum_sizegroup as
select distinct c.survey_year,stratum,c.district,
(meancpuewgt_male_le77 * total_area) bio_male_le77,
(meancpuewgt_male_le94 * total_area) bio_male_le94,
(meancpuewgt_male_95to101 * total_area) bio_male_95to101,
(meancpuewgt_male_ge25 * total_area) bio_male_ge25,
(meancpuewgt_male_ge78 * total_area) bio_male_ge78,
(meancpuewgt_male_ge95 * total_area) bio_male_ge95,
(meancpuewgt_male_ge102 * total_area) bio_male_ge102,
(meancpuewgt_male_total * total_area) bio_male_total,
(meancpuewgt_female_immature * total_area) bio_female_immature,
(meancpuewgt_female_mature * total_area) bio_female_mature,
(meancpuewgt_female_total * total_area) bio_female_total,
(meancpuewgt_gtotal * total_area) bio_gtotal
from co_meancpuewgt_sizegroup c, strata_opilio_newtimeseries s
where c.district = s.district
and c.survey_year = s.survey_year
order by survey_year,district;


drop table co_varcpuenum_sizegroup;

create table co_varcpuenum_sizegroup as
select c.survey_year,district,
VARIANCE (male_cpuenum_le77) varcpuenum_male_le77,
VARIANCE (male_cpuenum_le94) varcpuenum_male_le94,
VARIANCE (male_cpuenum_95to101) varcpuenum_male_95to101,
VARIANCE (male_cpuenum_ge25) varcpuenum_male_ge25,
VARIANCE (male_cpuenum_ge78) varcpuenum_male_ge78,
VARIANCE (male_cpuenum_ge95) varcpuenum_male_ge95,
VARIANCE (male_cpuenum_ge102) varcpuenum_male_ge102,
VARIANCE (male_cpuenum_total) varcpuenum_male_total,
VARIANCE (female_cpuenum_immature) varcpuenum_female_immature,
VARIANCE (female_cpuenum_mature) varcpuenum_female_mature,
VARIANCE (female_cpuenum_total) varcpuenum_female_total,
VARIANCE (unsexed_cpuenum_total) varcpuenum_unsexed_total,
VARIANCE (male_cpuenum_total + female_cpuenum_total + unsexed_cpuenum_total) varcpuenum_gtotal
from co_cpuenum_sizegroup c, strata_opilio_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;

drop table co_varcpuewgt_sizegroup;

create table co_varcpuewgt_sizegroup as
select c.survey_year,district,
VARIANCE (male_cpuewgt_le77) varcpuewgt_male_le77,
VARIANCE (male_cpuewgt_le94) varcpuewgt_male_le94,
VARIANCE (male_cpuewgt_95to101) varcpuewgt_male_95to101,
VARIANCE (male_cpuewgt_ge25) varcpuewgt_male_ge25,
VARIANCE (male_cpuewgt_ge78) varcpuewgt_male_ge78,
VARIANCE (male_cpuewgt_ge95) varcpuewgt_male_ge95,
VARIANCE (male_cpuewgt_ge102) varcpuewgt_male_ge102,
VARIANCE (male_cpuewgt_total) varcpuewgt_male_total,
VARIANCE (female_cpuewgt_immature) varcpuewgt_female_immature,
VARIANCE (female_cpuewgt_mature) varcpuewgt_female_mature,
VARIANCE (female_cpuewgt_total) varcpuewgt_female_total,
VARIANCE (male_cpuewgt_total + female_cpuewgt_total) varcpuewgt_gtotal
from co_cpuewgt_sizegroup c, strata_opilio_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;

drop table co_haulcount;


create table co_haulcount as
select count(hauljoin)number_tows, h.survey_year, district
from haul_newtimeseries_noretow h, strata_opilio_newtimeseries s
where h.gis_station = s.station_id
and h.survey_year = s.survey_year
and haul_type <> 17
group by h.survey_year, district;

drop table co_variancepop_sizegroup;

create table co_variancepop_sizegroup as
select distinct c.survey_year,stratum,c.district,
((varcpuenum_male_le77 * (power(total_area,2)))/number_tows) varpop_male_le77,
((varcpuenum_male_le94 * (power(total_area,2)))/number_tows) varpop_male_le94,
((varcpuenum_male_95to101 * (power(total_area,2)))/number_tows) varpop_male_95to101,
((varcpuenum_male_ge25 * (power(total_area,2)))/number_tows) varpop_male_ge25,
((varcpuenum_male_ge78 * (power(total_area,2)))/number_tows) varpop_male_ge78,
((varcpuenum_male_ge95 * (power(total_area,2)))/number_tows) varpop_male_ge95,
((varcpuenum_male_ge102 * (power(total_area,2)))/number_tows) varpop_male_ge102,
((varcpuenum_male_total * (power(total_area,2)))/number_tows) varpop_male_total,
((varcpuenum_female_immature * (power(total_area,2)))/number_tows) varpop_female_immature,
((varcpuenum_female_mature * (power(total_area,2)))/number_tows) varpop_female_mature,
((varcpuenum_female_total * (power(total_area,2)))/number_tows) varpop_female_total,
((varcpuenum_unsexed_total * (power(total_area,2)))/number_tows) varpop_unsexed_total,
((varcpuenum_gtotal * (power(total_area,2)))/number_tows) varpop_gtotal
from strata_opilio_newtimeseries s, co_varcpuenum_sizegroup c, co_haulcount n
where c.district = s.district
and c.district = n.district
and c.survey_year = s.survey_year
and c.survey_year = n.survey_year
order by c.survey_year,stratum;

drop table co_variancebio_sizegroup;

create table co_variancebio_sizegroup as
select distinct c.survey_year,stratum,c.district,
((varcpuewgt_male_le77 * (power(total_area,2)))/number_tows) varbio_male_le77,
((varcpuewgt_male_le94 * (power(total_area,2)))/number_tows) varbio_male_le94,
((varcpuewgt_male_95to101 * (power(total_area,2)))/number_tows) varbio_male_95to101,
((varcpuewgt_male_ge25 * (power(total_area,2)))/number_tows) varbio_male_ge25,
((varcpuewgt_male_ge78 * (power(total_area,2)))/number_tows) varbio_male_ge78,
((varcpuewgt_male_ge95 * (power(total_area,2)))/number_tows) varbio_male_ge95,
((varcpuewgt_male_ge102 * (power(total_area,2)))/number_tows) varbio_male_ge102,
((varcpuewgt_male_total * (power(total_area,2)))/number_tows) varbio_male_total,
((varcpuewgt_female_immature * (power(total_area,2)))/number_tows) varbio_female_immature,
((varcpuewgt_female_mature * (power(total_area,2)))/number_tows) varbio_female_mature,
((varcpuewgt_female_total * (power(total_area,2)))/number_tows) varbio_female_total,
((varcpuewgt_gtotal * (power(total_area,2)))/number_tows) varbio_gtotal
from strata_opilio_newtimeseries s, co_varcpuewgt_sizegroup c, co_haulcount n
where c.district = s.district
and c.district = n.district
and c.survey_year = s.survey_year
and c.survey_year = n.survey_year
order by c.survey_year,stratum;

-- Calculation by stock or district from this point on
-- For opilio, this will be total, east of 166W, 
-- between 166W and 173W, and west of 173W

drop table co_popall_sizegroup;

create table co_popall_sizegroup as
select survey_year,
sum(pop_male_le77) sum_pop_male_le77,
sum(pop_male_le94) sum_pop_male_le94,
sum(pop_male_95to101) sum_pop_male_95to101,
sum(pop_male_ge25) sum_pop_male_ge25,
sum(pop_male_ge78) sum_pop_male_ge78,
sum(pop_male_ge95) sum_pop_male_ge95,
sum(pop_male_ge102) sum_pop_male_ge102,
sum(pop_male_total) sum_pop_male_total,
sum(pop_female_immature) sum_pop_female_immature,
sum(pop_female_mature) sum_pop_female_mature,
sum(pop_female_total) sum_pop_female_total,
sum(pop_unsexed_total) sum_pop_unsexed_total,
sum(pop_gtotal) sum_pop_gtotal
from co_popbystratum_sizegroup
group by survey_year
order by survey_year;

drop table co_bioall_sizegroup;

create table co_bioall_sizegroup as
select survey_year,
sum(bio_male_le77) sum_bio_male_le77,
sum(bio_male_le94) sum_bio_male_le94,
sum(bio_male_95to101) sum_bio_male_95to101,
sum(bio_male_ge25) sum_bio_male_ge25,
sum(bio_male_ge78) sum_bio_male_ge78,
sum(bio_male_ge95) sum_bio_male_ge95,
sum(bio_male_ge102) sum_bio_male_ge102,
sum(bio_male_total) sum_bio_male_total,
sum(bio_female_immature) sum_bio_female_immature,
sum(bio_female_mature) sum_bio_female_mature,
sum(bio_female_total) sum_bio_female_total,
sum(bio_gtotal) sum_bio_gtotal
from co_biobystratum_sizegroup
group by survey_year
order by survey_year;


drop table co_varpop_sizegroup_sum;

create table co_varpop_sizegroup_sum as
select distinct survey_year,
sum(varpop_male_le77) sum_varpop_male_le77,
sum(varpop_male_le94) sum_varpop_male_le94,
sum(varpop_male_95to101) sum_varpop_male_95to101,
sum(varpop_male_ge25) sum_varpop_male_ge25,
sum(varpop_male_ge78) sum_varpop_male_ge78,
sum(varpop_male_ge95) sum_varpop_male_ge95,
sum(varpop_male_ge102) sum_varpop_male_ge102,
sum(varpop_male_total) sum_varpop_male_total,
sum(varpop_female_immature) sum_varpop_female_immature,
sum(varpop_female_mature) sum_varpop_female_mature,
sum(varpop_female_total) sum_varpop_female_total,
sum(varpop_unsexed_total) sum_varpop_unsexed_total,
sum(varpop_gtotal) sum_varpop_gtotal
from co_variancepop_sizegroup
group by survey_year
order by survey_year;

drop table co_varbio_sizegroup_sum;

create table co_varbio_sizegroup_sum as
select distinct survey_year,
sum(varbio_male_le77) sum_varbio_male_le77,
sum(varbio_male_le94) sum_varbio_male_le94,
sum(varbio_male_95to101) sum_varbio_male_95to101,
sum(varbio_male_ge25) sum_varbio_male_ge25,
sum(varbio_male_ge78) sum_varbio_male_ge78,
sum(varbio_male_ge95) sum_varbio_male_ge95,
sum(varbio_male_ge102) sum_varbio_male_ge102,
sum(varbio_male_total) sum_varbio_male_total,
sum(varbio_female_immature) sum_varbio_female_immature,
sum(varbio_female_mature) sum_varbio_female_mature,
sum(varbio_female_total) sum_varbio_female_total,
sum(varbio_gtotal) sum_varbio_gtotal
from co_variancebio_sizegroup
group by survey_year
order by survey_year;


drop table co_pop_sizegroup_cv;

create table co_pop_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_pop_male_le77 <> 0
	 then ((sqrt(sum_varpop_male_le77))/sum_pop_male_le77)
	 else 0
	 end) cv_pop_male_le77,
(CASE
	 when sum_pop_male_le94 <> 0
	 then ((sqrt(sum_varpop_male_le94))/sum_pop_male_le94)
	 else 0
	 end) cv_pop_male_le94,
(CASE
	 when sum_pop_male_95to101 <> 0
	 then ((sqrt(sum_varpop_male_95to101))/sum_pop_male_95to101)
	 else 0
	 end) cv_pop_male_95to101,
(CASE
	 when sum_pop_male_ge25 <> 0
	 then ((sqrt(sum_varpop_male_ge25))/sum_pop_male_ge25)
	 else 0
	 end) cv_pop_male_ge25,	    
(CASE
	 when sum_pop_male_ge78 <> 0
	 then ((sqrt(sum_varpop_male_ge78))/sum_pop_male_ge78)
	 else 0
	 end) cv_pop_male_ge78,	 
(CASE
	 when sum_pop_male_ge95 <> 0
	 then ((sqrt(sum_varpop_male_ge95))/sum_pop_male_ge95)
	 else 0
	 end) cv_pop_male_ge95,
(CASE
	 when sum_pop_male_ge102 <> 0
	 then ((sqrt(sum_varpop_male_ge102))/sum_pop_male_ge102)
	 else 0
	 end) cv_pop_male_ge102,	 	 
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
from co_varpop_sizegroup_sum a, co_popall_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;	 	 	 

drop table co_bio_sizegroup_cv;

create table co_bio_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_bio_male_le77 <> 0
	 then ((sqrt(sum_varbio_male_le77))/sum_bio_male_le77)
	 else 0
	 end) cv_bio_male_le77,
(CASE
	 when sum_bio_male_le94 <> 0
	 then ((sqrt(sum_varbio_male_le94))/sum_bio_male_le94)
	 else 0
	 end) cv_bio_male_le94,
(CASE
	 when sum_bio_male_95to101 <> 0
	 then ((sqrt(sum_varbio_male_95to101))/sum_bio_male_95to101)
	 else 0
	 end) cv_bio_male_95to101,
(CASE
	 when sum_bio_male_ge25 <> 0
	 then ((sqrt(sum_varbio_male_ge25))/sum_bio_male_ge25)
	 else 0
	 end) cv_bio_male_ge25,	    
(CASE
	 when sum_bio_male_ge78 <> 0
	 then ((sqrt(sum_varbio_male_ge78))/sum_bio_male_ge78)
	 else 0
	 end) cv_bio_male_ge78,	 
(CASE
	 when sum_bio_male_ge95 <> 0
	 then ((sqrt(sum_varbio_male_ge95))/sum_bio_male_ge95)
	 else 0
	 end) cv_bio_male_ge95,	 
(CASE
	 when sum_bio_male_ge102 <> 0
	 then ((sqrt(sum_varbio_male_ge102))/sum_bio_male_ge102)
	 else 0
	 end) cv_bio_male_ge102,	 	 
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
from co_varbio_sizegroup_sum a, co_bioall_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;

-- CI calcs

create or replace view co_sizegroup_stderr_pop as
select distinct survey_year,
(sqrt(sum_varpop_male_le77)) stderr_pop_male_le77,	 	 	 
(sqrt(sum_varpop_male_le94)) stderr_pop_male_le94,
(sqrt(sum_varpop_male_95to101)) stderr_pop_male_95to101,
(sqrt(sum_varpop_male_ge25)) stderr_pop_male_ge25,
(sqrt(sum_varpop_male_ge78)) stderr_pop_male_ge78,
(sqrt(sum_varpop_male_ge95)) stderr_pop_male_ge95,
(sqrt(sum_varpop_male_ge102)) stderr_pop_male_ge102,
(sqrt(sum_varpop_male_total)) stderr_pop_male_total,
(sqrt(sum_varpop_female_immature)) stderr_pop_female_immature,
(sqrt(sum_varpop_female_mature)) stderr_pop_female_mature,
(sqrt(sum_varpop_female_total)) stderr_pop_female_total,
(sqrt(sum_varpop_unsexed_total)) stderr_pop_unsexed_total,
(sqrt(sum_varpop_gtotal)) stderr_pop_gtotal
from co_varpop_sizegroup_sum;

drop table co_sizegroup_confidence_pop;

create table co_sizegroup_confidence_pop as
select distinct survey_year,
(1.96 * stderr_pop_male_le77) ci_pop_male_le77,
(1.96 * stderr_pop_male_le94) ci_pop_male_le94,
(1.96 * stderr_pop_male_95to101) ci_pop_male_95to101,
(1.96 * stderr_pop_male_ge25) ci_pop_male_ge25,
(1.96 * stderr_pop_male_ge78) ci_pop_male_ge78,
(1.96 * stderr_pop_male_ge95) ci_pop_male_ge95,
(1.96 * stderr_pop_male_ge102) ci_pop_male_ge102,
(1.96 * stderr_pop_male_total) ci_pop_male_total,
(1.96 * stderr_pop_female_immature) ci_pop_female_immature,
(1.96 * stderr_pop_female_mature) ci_pop_female_mature,
(1.96 * stderr_pop_female_total) ci_pop_female_total,
(1.96 * stderr_pop_unsexed_total) ci_pop_unsexed_total,
(1.96 * stderr_pop_gtotal) ci_pop_gtotal
from co_sizegroup_stderr_pop;


create or replace view co_sizegroup_stderr_bio as
select distinct survey_year,
(sqrt(sum_varbio_male_le77)) stderr_bio_male_le77,	 	 	 
(sqrt(sum_varbio_male_le94)) stderr_bio_male_le94,
(sqrt(sum_varbio_male_95to101)) stderr_bio_male_95to101,
(sqrt(sum_varbio_male_ge25)) stderr_bio_male_ge25,
(sqrt(sum_varbio_male_ge78)) stderr_bio_male_ge78,
(sqrt(sum_varbio_male_ge95)) stderr_bio_male_ge95,
(sqrt(sum_varbio_male_ge102)) stderr_bio_male_ge102,
(sqrt(sum_varbio_male_total)) stderr_bio_male_total,
(sqrt(sum_varbio_female_immature)) stderr_bio_female_immature,
(sqrt(sum_varbio_female_mature)) stderr_bio_female_mature,
(sqrt(sum_varbio_female_total)) stderr_bio_female_total,
(sqrt(sum_varbio_gtotal)) stderr_bio_gtotal
from co_varbio_sizegroup_sum;

drop table co_sizegroup_confidence_bio;

create table co_sizegroup_confidence_bio as
select distinct survey_year,
((1.96 * stderr_bio_male_le77)) ci_bio_male_le77,
((1.96 * stderr_bio_male_le94)) ci_bio_male_le94,
((1.96 * stderr_bio_male_95to101)) ci_bio_male_95to101,
((1.96 * stderr_bio_male_ge25)) ci_bio_male_ge25,
((1.96 * stderr_bio_male_ge78)) ci_bio_male_ge78,
((1.96 * stderr_bio_male_ge95)) ci_bio_male_ge95,
((1.96 * stderr_bio_male_ge102)) ci_bio_male_ge102,
((1.96 * stderr_bio_male_total)) ci_bio_male_total,
((1.96 * stderr_bio_female_immature)) ci_bio_female_immature,
((1.96 * stderr_bio_female_mature)) ci_bio_female_mature,
((1.96 * stderr_bio_female_total)) ci_bio_female_total,
((1.96 * stderr_bio_gtotal)) ci_bio_gtotal
from co_sizegroup_stderr_bio;


-- Final output for stocks

drop table co_bio_matfem_newmodel_ML;

create table co_bio_matfem_newmodel_ML as
select a.survey_year,
--sum_bio_male_le77 biomass_male_le77,cv_bio_male_le77 cv_biomass_male_le77,ci_bio_male_le77 ci_biomass_male_le77,
sum_bio_male_ge25 biomass_male_ge25,cv_bio_male_ge25 cv_biomass_male_ge25,ci_bio_male_ge25 ci_biomass_male_ge25,
sum_bio_male_le94 biomass_male_le94,cv_bio_male_le94 cv_biomass_male_le94,ci_bio_male_le94 ci_biomass_male_le94,
sum_bio_male_ge95 biomass_male_ge95,cv_bio_male_ge95 cv_biomass_male_ge95,ci_bio_male_ge95 ci_biomass_male_ge95,
sum_bio_male_ge78 biomass_male_ge78,cv_bio_male_ge78 cv_biomass_male_ge78,ci_bio_male_ge78 ci_biomass_male_ge78,
sum_bio_male_ge102 biomass_male_ge102,cv_bio_male_ge102 cv_biomass_male_ge102,ci_bio_male_ge102 ci_biomass_male_ge102,
sum_bio_male_95to101 biomass_male_95to101,cv_bio_male_95to101 cv_biomass_male_95to101,ci_bio_male_95to101 ci_biomass_male_95to101,
sum_bio_male_total biomass_male_total,cv_bio_male_total cv_biomass_male_total,ci_bio_male_total ci_biomass_male_total,
sum_bio_female_immature biomass_female_immature,cv_bio_female_immature cv_biomass_female_immature,ci_bio_female_immature ci_biomass_female_immature,
sum_bio_female_mature biomass_female_mature,cv_bio_female_mature cv_biomass_female_mature,ci_bio_female_mature ci_biomass_female_mature,
sum_bio_female_total biomass_female_total,cv_bio_female_total cv_biomass_female_total,ci_bio_female_total ci_biomass_female_total
--sum_bio_gtotal gtotal_mt,cv_bio_gtotal cv_gtotal_mt,ci_bio_gtotal ci_gtotal_mt
from co_bioall_sizegroup a, co_sizegroup_confidence_bio b,co_bio_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
--and a.survey_year = 2013
order by a.survey_year;

/*
drop table co_pop_matfem_newmodel;

create table co_pop_matfem_newmodel as
select a.survey_year,
sum_pop_male_ge25 num_male_ge25,cv_pop_male_ge25 cv_num_male_ge25,ci_pop_male_ge25 ci_num_male_ge25,
sum_pop_male_le94 num_male_le94,cv_pop_male_le94 cv_num_male_le94,ci_pop_male_le94 ci_num_male_le94,
sum_pop_male_ge95 num_male_ge95,cv_pop_male_ge95 cv_num_male_ge95,ci_pop_male_ge95 ci_num_male_ge95,
sum_pop_male_ge78 num_male_ge78,cv_pop_male_ge78 cv_num_male_ge78,ci_pop_male_ge78 ci_num_male_ge78,
sum_pop_male_ge102 num_male_ge102,cv_pop_male_ge102 cv_num_male_ge102,ci_pop_male_ge102 ci_num_male_ge102,
sum_pop_male_95to101 num_male_95to101,cv_pop_male_95to101 cv_num_male_95to101,ci_pop_male_95to101 ci_num_male_95to101,
sum_pop_male_total num_male_total,cv_pop_male_total cv_num_male_total,ci_pop_male_total ci_num_male_total,
sum_pop_female_immature num_female_immature,cv_pop_female_immature cv_num_female_immature,ci_pop_female_immature ci_num_female_immature,
sum_pop_female_mature num_female_mature,cv_pop_female_mature cv_num_female_mature,ci_pop_female_mature ci_num_female_mature,
sum_pop_female_total num_female_total,cv_pop_female_total cv_num_female_total,ci_pop_female_total ci_num_female_total
--sum_pop_gtotal gtotal,cv_pop_gtotal cv_gtotal,ci_pop_gtotal ci_gtotal
from co_popall_sizegroup a, co_sizegroup_confidence_pop b,co_pop_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
--and a.survey_year = 2013
order by a.survey_year;

