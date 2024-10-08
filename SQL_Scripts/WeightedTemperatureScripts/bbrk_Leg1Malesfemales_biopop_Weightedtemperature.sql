-- This script produces a table of population estimates for Bristol Bay red king
-- crab from the 1975 - 2019 EBS trawl surveys.  No retow data is included in this run.
-- Population is calculated for males, females, and unsexed crab by size category or maturity.

-- This script requires as input the master crab table (ebscrab) populated
-- with the survey data to be analyzed; a subset of the racebase.haul
-- table containing the haul data for the cruises being analyzed; 
-- and a strata lookup table.
-------------------------------------------------------------------------------

-- 
-- Don't include BB retows, get rid of haul type 17 tows (first pass @ station only)

drop table haul_newtimeseries_noretow;

create table haul_newtimeseries_noretow as
select * from haul_newtimeseries
where haul_type <> 17;



-- Create tables of raw catch by 1-mm size bin and sex
-- Separate by sex because male size group categories require shell condition
-- and female weights (post-2009) require clutch size

drop table rk_number_size1_male;

create table rk_number_size1_male as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,
shell_condition,(trunc(length/1) * 1)size1,
(sum(CASE
		 when species_code = 69322
		 and sex = 1
		 then sampling_factor
		 else 0
		 end)) number_male_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 69322
and length <> 999
and c.hauljoin(+) = h.hauljoin
and haul_type <> 17
group by c.hauljoin,
	  	 c.vessel,
		 c.cruise,
		 c.haul,
		 h.gis_station,
		 species_code,
     shell_condition,
		 (trunc(length/1) * 1);


-- females (done separately from males because need clutch size info)

drop table rk_number_size1_female;

create table rk_number_size1_female as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,clutch_size,
(trunc(length/1) * 1)size1,
(sum(CASE
		 when species_code = 69322
		 and sex = 2
		 then sampling_factor
		 else 0
		 end)) number_female_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 69322
and length <> 999
and c.hauljoin(+) = h.hauljoin
and haul_type <> 17
group by c.hauljoin,
	  	 c.vessel,
		 c.cruise,
		 c.haul,
		 h.gis_station,
		 species_code,
     clutch_size,
		 (trunc(length/1) * 1);


-- unsexed

drop table rk_number_size1_unsexed;

create table rk_number_size1_unsexed as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,
(trunc(length/1) * 1)size1,
(sum(CASE
		 when species_code = 69322
		 and sex = 3
		 then sampling_factor
		 else 0
		 end)) number_unsexed_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 69322
and length <> 999
and c.hauljoin(+) = h.hauljoin
and haul_type <> 17
group by c.hauljoin,
	  	 c.vessel,
		 c.cruise,
		 c.haul,
		 h.gis_station,
		 species_code,
		 (trunc(length/1) * 1);



--  This section calculates the weight of the red king crab by haul, sex,
--  shell condition and 1-mm size group.  A length-weight regression
--  factor is applied, and multiplied by the number of crab caught in that
--  haul/sex/shellcon/size bin (from above section).  This method ensures
--  that a weight for the re-apportioned unmeasured crab is accounted for.
--  The regression factor does not include unsexed crab, therefore no weights
--  will be calculated for unsexed crab

drop table rk_weight_grams_male;

create table rk_weight_grams_male as
select a.hauljoin,a.vessel,a.cruise,a.haul,a.gis_station,species_code,size1,shell_condition,
(CASE
--    WHEN a.cruise < 201001
--      THEN ((0.00036 * (power(size1,3.16))) * number_male_size1)
    WHEN a.cruise >= 197501
      THEN ((0.000403 * (power(size1,3.141334))) * number_male_size1)
    ELSE 0
    END) wgt_male_size1
from rk_number_size1_male a, haul_newtimeseries_noretow b
where species_code = 69322 
and a.hauljoin(+) = b.hauljoin
order by a.cruise,a.vessel,a.haul,a.gis_station,size1;

drop table rk_weight_grams_female;

create table rk_weight_grams_female as
select a.hauljoin,a.vessel,a.cruise,a.haul,a.gis_station,species_code,size1,
(CASE
 --   WHEN a.cruise < 201001
 --     THEN ((0.01027 * (power(size1,2.38849))) * number_female_size1)
    WHEN a.cruise >= 197501 and clutch_size <= 1
      THEN ((0.000408 * (power(size1,3.127956))) * number_female_size1)
    WHEN a.cruise >= 197501 and clutch_size > 1
      THEN ((0.003593 * (power(size1,2.666076))) * number_female_size1)
    ELSE 0
    END) wgt_female_size1
from rk_number_size1_female a, haul_newtimeseries_noretow b
where species_code = 69322 
and a.hauljoin(+) = b.hauljoin
order by a.cruise,a.vessel,a.haul,a.gis_station,size1;

-- combine male and female weight tables

drop table rk_weight_grams_size1;

create table rk_weight_grams_size1 
( HAULJOIN               NUMBER(12),
  VESSEL                 NUMBER(4),
  CRUISE                 NUMBER(6),
  HAUL                   NUMBER(4),
  GIS_STATION            VARCHAR2(10),
  SPECIES_CODE           NUMBER(6),
  SIZE1                  NUMBER,
  SHELL_CONDITION        NUMBER,
  WGT_MALE_SIZE1         NUMBER,
  WGT_FEMALE_SIZE1       NUMBER
);
insert into rk_weight_grams_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
wgt_male_size1,null
from rk_weight_grams_male;

insert into rk_weight_grams_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,wgt_female_size1
from rk_weight_grams_female;

-- convert to metric tons

drop table rk_weight_mt_size1;

create table rk_weight_mt_size1 as
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
(wgt_male_size1 * 0.000001) mt_male_size1,
(wgt_female_size1 * 0.000001) mt_female_size1
from rk_weight_grams_size1
order by cruise,vessel,haul,gis_station,size1;

-- Combine the reapportioned male, unsexed, and female by number tables

drop table rk_number_size1;

create table rk_number_size1 
( HAULJOIN               NUMBER(12),
  VESSEL                 NUMBER(4),
  CRUISE                 NUMBER(6),
  HAUL                   NUMBER(4),
  GIS_STATION            VARCHAR2(10),
  SPECIES_CODE           NUMBER(6),
  SIZE1                  NUMBER,
  SHELL_CONDITION        NUMBER,
  NUMBER_MALE_SIZE1      NUMBER,
  NUMBER_FEMALE_SIZE1    NUMBER,
  NUMBER_UNSEXED_SIZE1   NUMBER
);
insert into rk_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
number_male_size1,null,null
from rk_number_size1_male;

insert into rk_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,number_female_size1,null
from rk_number_size1_female;

insert into rk_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,null,number_unsexed_size1
from rk_number_size1_unsexed;


-- This section sums the red king crab catch records by haul, sex,
-- shell condition, and 1-mm size group.  

drop table rk_number_sizegroup_temp;

create table rk_number_sizegroup_temp as
select hauljoin, vessel, cruise, haul, gis_station, species_code,
	   (sum(CASE
	   			WHEN  size1 between 0 and 94.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le94,
	   (sum(CASE
	   			WHEN  size1 between 0 and 104.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le104,
	   (sum(CASE
	   			WHEN  size1 between 0 and 109.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le109,
	   (sum(CASE
	   			WHEN  size1 between 0 and 119.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le119,	
	   (sum(CASE
	   			WHEN  size1 between 0 and 119.9
          and shell_condition in (1,2)
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le119_newshell,	        
	   (sum(CASE
	   			WHEN  size1 between 95.0 and 109.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_95to109,				  
	   (sum(CASE
	   			WHEN  size1 between 105.0 and 119.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_105to119,
	   (sum(CASE
	   			WHEN  size1 between 110.0 and 134.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_110to134,				
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 134.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_120to134,	
	   (sum(CASE
	   			WHEN  size1 between 65.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge65,				
	   (sum(CASE
	   			WHEN  size1 between 105.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge105,				
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge120,
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 250
          and shell_condition in (1,2)
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge120_newshell, 
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 250
          and shell_condition in (0,3,4,5)
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge120_oldshell,                 
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge135,							
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 149.9
				AND shell_condition in (1,2)
			    THEN number_male_size1
				ELSE 0
				END))  number_recruit_prib,
	   (sum(CASE
	   			WHEN  size1 between 150.0 and 250
				AND shell_condition in (1,2)
			    THEN number_male_size1
				ELSE 0
				END))  number_ge150_newshell,				
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 250
				AND shell_condition in (0,3,4,5)
			    THEN number_male_size1
				ELSE 0
				END))  number_ge135_oldshell,
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_total,		
	   (sum(CASE
	   			WHEN  size1 between 0 and 89.9
			    THEN number_female_size1
				ELSE 0
				END))  number_female_le89,									
	   (sum(CASE
	   			WHEN  size1 between 90.0 and 250
			    THEN number_female_size1
				ELSE 0
				END))  number_female_ge90,	
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN number_female_size1
				ELSE 0
				END))  number_female_total,
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN number_unsexed_size1
				ELSE 0
				END))  number_unsexed_total															
	   from rk_number_size1
         where species_code = 69322
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code;
				
drop table rk_number_sizegroup;

create table rk_number_sizegroup as
select hauljoin,vessel,cruise,haul,gis_station,species_code,
number_male_le94,number_male_le104,number_male_le109,number_male_le119,
number_male_le119_newshell,
number_male_95to109,number_male_105to119,number_male_110to134,number_male_120to134,
number_male_ge65,number_male_ge105,number_male_ge120,
number_male_ge120_newshell,number_male_ge120_oldshell,
number_male_ge135,number_recruit_prib,
(number_ge150_newshell + number_ge135_oldshell) number_pr_prib,
number_male_total,
number_female_le89,number_female_ge90,
number_female_total,number_unsexed_total
from rk_number_sizegroup_temp
order by cruise,vessel,haul,gis_station;			
				

drop table rk_weight_mt_sizegroup_temp;

create table rk_weight_mt_sizegroup_temp as
select hauljoin, vessel, cruise, haul, gis_station, species_code,
	   (sum(CASE
	   			WHEN  size1 between 0 and 94.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le94,
	   (sum(CASE
	   			WHEN  size1 between 0 and 104.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le104,
	   (sum(CASE
	   			WHEN  size1 between 0 and 109.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le109,
	   (sum(CASE
	   			WHEN  size1 between 0 and 119.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le119,	
	   (sum(CASE
	   			WHEN  size1 between 0 and 119.9
          and shell_condition in (1,2)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le119_newshell,	                
	   (sum(CASE
	   			WHEN  size1 between 95.0 and 109.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_95to109,				  
	   (sum(CASE
	   			WHEN  size1 between 105.0 and 119.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_105to119,
	   (sum(CASE
	   			WHEN  size1 between 110.0 and 134.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_110to134,				
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 134.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_120to134,	
	   (sum(CASE
	   			WHEN  size1 between 65.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge65,				
	   (sum(CASE
	   			WHEN  size1 between 105.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge105,				
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge120,
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 250
          and shell_condition in (1,2)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge120_newshell, 
	   (sum(CASE
	   			WHEN  size1 between 120.0 and 250
          and shell_condition in (0,3,4,5)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge120_oldshell,        
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge135,							
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 149.9
				AND shell_condition in (1,2)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_recruit_prib,
	   (sum(CASE
	   			WHEN  size1 between 150.0 and 250
				AND shell_condition in (1,2)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_ge150_newshell,				
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 250
				AND shell_condition in (0,3,4,5)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_ge135_oldshell,
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_total,		
	   (sum(CASE
	   			WHEN  size1 between 0 and 89.9
			    THEN mt_female_size1
				ELSE 0
				END))  mt_female_le89,									
	   (sum(CASE
	   			WHEN  size1 between 90.0 and 250
			    THEN mt_female_size1
				ELSE 0
				END))  mt_female_ge90,	
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN mt_female_size1
				ELSE 0
				END))  mt_female_total											
	   from rk_weight_mt_size1
         where species_code = 69322
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code;
				
drop table rk_weight_mt_sizegroup;

create table rk_weight_mt_sizegroup as
select hauljoin,vessel,cruise,haul,gis_station,species_code,
mt_male_le94,mt_male_le104,mt_male_le109,mt_male_le119,mt_male_le119_newshell,
mt_male_95to109,mt_male_105to119,mt_male_110to134,mt_male_120to134,
mt_male_ge65,mt_male_ge105,mt_male_ge120,
mt_male_ge120_newshell,mt_male_ge120_oldshell,
mt_male_ge135,mt_recruit_prib,
(mt_ge150_newshell + mt_ge135_oldshell) mt_pr_prib,
mt_male_total,
mt_female_le89,mt_female_ge90,
mt_female_total
from rk_weight_mt_sizegroup_temp
order by cruise,vessel,haul,gis_station;
				
				

-- This section combines the haul and catch data, including
-- those haul/size groups where there was no catch.				

drop table rk_num_sizegroup_union;

create table rk_num_sizegroup_union as
select h.hauljoin,h.vessel,h.cruise,h.haul,h.gis_station,survey_year,h.gear_depth,h.surface_temperature,h.gear_temperature,
nvl(species_code,69322) species_code,
nvl(number_male_le94,0) number_male_le94,
nvl(number_male_le104,0) number_male_le104,
nvl(number_male_le109,0) number_male_le109,
nvl(number_male_le119,0) number_male_le119,
nvl(number_male_le119_newshell,0) number_male_le119_newshell,
nvl(number_male_95to109,0) number_male_95to109,
nvl(number_male_105to119,0) number_male_105to119,
nvl(number_male_110to134,0) number_male_110to134,
nvl(number_male_120to134,0) number_male_120to134,
nvl(number_male_ge65,0) number_male_ge65,
nvl(number_male_ge105,0) number_male_ge105,
nvl(number_male_ge120,0) number_male_ge120,
nvl(number_male_ge120_newshell,0) number_male_ge120_newshell,
nvl(number_male_ge120_oldshell,0) number_male_ge120_oldshell,
nvl(number_male_ge135,0) number_male_ge135,
nvl(number_recruit_prib,0) number_recruit_prib,
nvl(number_pr_prib,0) number_pr_prib,
nvl(number_male_total,0) number_male_total,
nvl(number_female_le89,0) number_female_le89,
nvl(number_female_ge90,0) number_female_ge90,
nvl(number_female_total,0) number_female_total,
nvl(number_unsexed_total,0) number_unsexed_total
from haul_newtimeseries_noretow h full outer join rk_number_sizegroup c
on h.hauljoin = c.hauljoin;

drop table vast_bbrk_l1male_num_sizegrp;

create table vast_bbrk_l1male_num_sizegrp as 
select hauljoin, vessel, cruise, haul, gis_station,survey_year,gear_depth,surface_temperature,gear_temperature,number_male_le119,number_male_ge120,number_male_total,number_female_mature,number_female_immature, number_female_total 
from rk_num_sizegroup_union c where c.gis_station in(select station_id from centers_bristolbay);
--  Similarly, by weight.

drop table rk_wgt_sizegroup_union;

create table rk_wgt_sizegroup_union as
select h.hauljoin,h.vessel,h.cruise,h.haul,h.gis_station,survey_year,
nvl(species_code,69322) species_code,
nvl(mt_male_le94,0) mt_male_le94,
nvl(mt_male_le104,0) mt_male_le104,
nvl(mt_male_le109,0) mt_male_le109,
nvl(mt_male_le119,0) mt_male_le119,
nvl(mt_male_le119_newshell,0) mt_male_le119_newshell,
nvl(mt_male_95to109,0) mt_male_95to109,
nvl(mt_male_105to119,0) mt_male_105to119,
nvl(mt_male_110to134,0) mt_male_110to134,
nvl(mt_male_120to134,0) mt_male_120to134,
nvl(mt_male_ge65,0) mt_male_ge65,
nvl(mt_male_ge105,0) mt_male_ge105,
nvl(mt_male_ge120,0) mt_male_ge120,
nvl(mt_male_ge120_newshell,0) mt_male_ge120_newshell,
nvl(mt_male_ge120_oldshell,0) mt_male_ge120_oldshell,
nvl(mt_male_ge135,0) mt_male_ge135,
nvl(mt_recruit_prib,0) mt_recruit_prib,
nvl(mt_pr_prib,0) mt_pr_prib,
nvl(mt_male_total,0) mt_male_total,
nvl(mt_female_le89,0) mt_female_le89,
nvl(mt_female_ge90,0) mt_female_ge90,
nvl(mt_female_total,0) mt_female_total
from haul_newtimeseries_noretow h full outer join rk_weight_mt_sizegroup c
on h.hauljoin = c.hauljoin;


-- This section calculates cpue for each haul.
-- If a station contains multiple tows, cpue
-- is calculated for each of the tows, not averaged for the station.
-- A value, even if 0 for no catch, is output for every size group,
-- every haul.  CPUE is calculated as number of crabs per square
-- nautical mile towed; area swept is the distance fished multiplied
-- by the actual (measured) net width.

drop table rk_cpuenum_sizegroup;

create table rk_cpuenum_sizegroup as
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,
c.gis_station,c.survey_year,c.species_code,h.gear_depth,h.surface_temperature,h.gear_temperature,
(number_male_le94 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le94,
(number_male_le104 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le104,
(number_male_le109 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le109,
(number_male_le119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le119,
(number_male_le119_newshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le119_newshell,
(number_male_95to109 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_95to109,
(number_male_105to119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_105to119,
(number_male_110to134 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_110to134,
(number_male_120to134 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_120to134,
(number_male_ge65 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge65,
(number_male_ge105 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge105,
(number_male_ge120 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge120,
(number_male_ge120_newshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge120_newshell,
(number_male_ge120_oldshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge120_oldshell,
(number_male_ge135 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge135,
(number_recruit_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_recruit_prib,
(number_pr_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_pr_prib,
(number_male_total / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_total,
(number_female_le89 / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_le89,
(number_female_ge90 / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_ge90,
(number_female_total / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_total,
(number_unsexed_total / (((net_width/1000) * distance_fished) * 0.29155335)) unsexed_cpuenum_total
from rk_num_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin;

-- NOTE: weight has already been converted to metric tons, so units here = mt/nm2

drop table rk_cpuewgt_sizegroup;

create table rk_cpuewgt_sizegroup as
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,
c.gis_station,c.survey_year,c.species_code,h.gear_depth,h.surface_temperature,h.gear_temperature,
(mt_male_le94 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le94,
(mt_male_le104 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le104,
(mt_male_le109 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le109,
(mt_male_le119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le119,
(mt_male_le119_newshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le119_newshell,
(mt_male_95to109 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_95to109,
(mt_male_105to119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_105to119,
(mt_male_110to134 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_110to134,
(mt_male_120to134 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_120to134,
(mt_male_ge65 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge65,
(mt_male_ge105 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge105,
(mt_male_ge120 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge120,
(mt_male_ge120_newshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge120_newshell,
(mt_male_ge120_oldshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge120_oldshell,
(mt_male_ge135 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge135,
(mt_recruit_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_recruit_prib,
(mt_pr_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_pr_prib,
(mt_male_total / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_total,
(mt_female_le89 / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_le89,
(mt_female_ge90 / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_ge90,
(mt_female_total / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_total
from rk_wgt_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin;

drop table rk_cpuenum_weighted_temp;

create table rk_cpuenum_weighted_temp as 
select survey_year,cruise,
sum(gear_temperature * male_cpuenum_total)/sum(male_cpuenum_total) male_weighted_gear_temp,
sum(gear_temperature * female_cpuenum_total)/sum(female_cpuenum_total) female_weighted_gear_temp
from rk_cpuenum_sizegroup
group by survey_year, cruise;


select * from RK_CPUENUM_WEIGHTED_TEMP;