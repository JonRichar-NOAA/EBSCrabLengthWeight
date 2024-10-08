-- This script produces a table of population estimates for blue king
-- crab from the 1975-2016 EBS trawl surveys.  Population is calculated
-- for males and female crab by stock assessment size category or actual maturity for each district. 
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

drop table bk_number_size1_male;

create table bk_number_size1_male as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,
shell_condition,(trunc(length/1) * 1)size1,
(sum(CASE
		 when species_code = 69323
		 and sex = 1
		 then sampling_factor
		 else 0
		 end)) number_male_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 69323
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


-- Females (done separately from males because need clutch size info)
-- as of 2016, clutch size info is not factored into bkc female size-weight
-- regression, but leaving this in as place holder just in case!

drop table bk_number_size1_female;

create table bk_number_size1_female as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,clutch_size,
(trunc(length/1) * 1)size1,
(sum(CASE
		 when species_code = 69323
		 and sex = 2
		 then sampling_factor
		 else 0
		 end)) number_female_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 69323
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

drop table bk_number_size1_unsexed;

create table bk_number_size1_unsexed as
select c.hauljoin,c.vessel,c.cruise,c.haul,h.gis_station,species_code,
(trunc(length/1) * 1)size1,
(sum(CASE
		 when species_code = 69323
		 and sex = 3
		 then sampling_factor
		 else 0
		 end)) number_unsexed_size1
from crab.ebscrab c, haul_newtimeseries_noretow h
where species_code = 69323
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


--  This section calculates the weight of the blue king crab by haul, sex,
--  shell condition and 1-mm size group.  A length-weight regression
--  factor is applied, and multiplied by the number of crab caught in that
--  haul/sex/shellcon/size bin (from above section).  
--  The regression factor does not include unsexed crab, therefore no weights
--  will be calculated for unsexed crab

drop table bk_weight_grams_male;

create table bk_weight_grams_male as
select a.hauljoin,a.vessel,a.cruise,a.haul,a.gis_station,species_code,size1,shell_condition,
-- pribilof bkc
	(CASE
--		WHEN mid_latitude < 58.65 and a.cruise < 201001
--			THEN
--				 ((0.00047 * (power(size1,3.103))) * number_male_size1)
    WHEN mid_latitude < 58.65 and a.cruise >= 197501
			THEN
				 ((0.000508 * (power(size1,3.106409))) * number_male_size1)  
-- st matt bkc
--    WHEN mid_latitude > 58.65 and a.cruise < 201001
--			THEN
--				 ((0.00033 * (power(size1,3.175))) * number_male_size1)
    WHEN mid_latitude > 58.65 and a.cruise >= 197501
    AND shell_condition <= 2
			THEN
				 ((0.000346 * (power(size1,3.176559))) * number_male_size1)
    WHEN mid_latitude > 58.65 and a.cruise >= 197501
    AND shell_condition > 2
			THEN
				 ((0.000552 * (power(size1,3.093953))) * number_male_size1)
		ELSE 0
		END)  wgt_male_size1
from bk_number_size1_male a, haul_newtimeseries_noretow b
where species_code = 69323 
and a.hauljoin(+) = b.hauljoin
order by a.cruise,a.vessel,a.haul,a.gis_station,size1;

drop table bk_weight_grams_female;

create table bk_weight_grams_female as
select a.hauljoin,a.vessel,a.cruise,a.haul,a.gis_station,species_code,size1,clutch_size,
-- female bkc, same calcs for Prib and St Matt, all years
	(CASE
    WHEN mid_latitude < 58.65
			THEN
				 ((0.02065 * (power(size1,2.27))) * number_female_size1)
    WHEN mid_latitude > 58.65
        AND clutch_size = 0 
			THEN
				 ((0.000226 * (power(size1,3.272900))) * number_female_size1)
    WHEN mid_latitude > 58.65
        AND clutch_size >= 1 
			THEN
				 ((0.000172 * (power(size1,3.341792))) * number_female_size1)
		ELSE 0
		END)  wgt_female_size1
from bk_number_size1_female a, haul_newtimeseries_noretow b
where species_code = 69323 
and a.hauljoin(+) = b.hauljoin
order by a.cruise,a.vessel,a.haul,a.gis_station,size1;

-- Using actual female maturity in this run, so select for clutch size here

drop table bk_number_size1_matfem;

create table bk_number_size1_matfem as
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
    from bk_number_size1_female
    where species_code = 69323
		 --and length <> 999
	     --and hauljoin(+) = h.hauljoin
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code,
				size1;
        
-- And calculate weight of crab by actual maturity        

drop table bk_weight_grams_matfem;

create table bk_weight_grams_matfem as
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
    from bk_weight_grams_female
    where species_code = 69323  
		 --and length <> 999
	     --and hauljoin(+) = h.hauljoin
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code,
				size1;
        

-- combine male and female weight tables

drop table bk_weight_grams_size1;

create table bk_weight_grams_size1 
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
insert into bk_weight_grams_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
wgt_male_size1,null,null
from bk_weight_grams_male;

insert into bk_weight_grams_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,wgt_female_size1_immature,wgt_female_size1_mature
from bk_weight_grams_matfem;

-- convert to metric tons

drop table bk_weight_mt_size1;

create table bk_weight_mt_size1 as
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
(wgt_male_size1 * 0.000001) mt_male_size1,
(wgt_female_size1_immature * 0.000001) mt_female_size1_immature,
(wgt_female_size1_mature * 0.000001) mt_female_size1_mature
from bk_weight_grams_size1
order by cruise,vessel,haul,gis_station,size1;

-- Combine the male, female, and unsexed by number tables

drop table bk_number_size1;

create table bk_number_size1 
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
insert into bk_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,shell_condition,
number_male_size1,null,null,null
from bk_number_size1_male;

insert into bk_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,number_female_size1_immature,number_female_size1_mature,null
from bk_number_size1_matfem;

insert into bk_number_size1
select hauljoin,vessel,cruise,haul,gis_station,species_code,size1,null,
null,null,null,number_unsexed_size1
from bk_number_size1_unsexed;


-- This section sums the blue king crab catch records by haul, sex,
-- shell condition, and 1-mm size group for the size categories needed for 
-- ADF&G and stock assessment. 

drop table bk_number_sizegroup_temp;

create table bk_number_sizegroup_temp as
select hauljoin, vessel, cruise, haul, gis_station, species_code,
	   (sum(CASE
	   			WHEN  size1 between 0 and 89.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_le89,
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
	   			WHEN  size1 between 90.0 and 104.9
			    THEN number_male_size1
				ELSE 0
				END))  number_male_90to104,				  
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
	   			WHEN  size1 between 90.0 and 250
			    THEN number_male_size1
				ELSE 0
				END))  number_male_ge90,				
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
	   			WHEN  size1 between 120.0 and 133
				AND shell_condition in (1,2)
			    THEN number_male_size1
				ELSE 0
				END))  number_recruit_stmatt,	
	   (sum(CASE
	   			WHEN  size1 between 134.0 and 250
				AND shell_condition in (1,2)
			    THEN number_male_size1
				ELSE 0
				END))  number_ge134_newshell,	        
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 250
				AND shell_condition in (1,2)
			    THEN number_male_size1
				ELSE 0
				END))  number_ge135_newshell,				
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 149
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
	   sum(number_female_size1_immature) number_female_immature,
     sum(number_female_size1_mature) number_female_mature,
	   (sum(CASE
	   			WHEN  size1 between 0 and 250
			    THEN number_unsexed_size1
				ELSE 0
				END))  number_unsexed_total															
	   from bk_number_size1
         where species_code = 69323
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code;
				
drop table bk_number_sizegroup;

create table bk_number_sizegroup as
select hauljoin,vessel,cruise,haul,gis_station,species_code,
number_male_le89,number_male_le104,number_male_le109,number_male_le119,
number_male_le119_newshell,number_male_90to104,number_male_105to119,number_male_110to134,
number_male_120to134,number_male_ge90,number_male_ge105,number_male_ge120,
number_male_ge120_newshell,number_male_ge120_oldshell,
number_male_ge135,number_recruit_stmatt,number_recruit_prib,
(number_ge134_newshell + number_male_ge120_oldshell) number_pr_stmatt,
(number_ge150_newshell + number_ge135_oldshell) number_pr_prib,
((number_ge135_newshell + number_male_ge120_oldshell) + number_recruit_stmatt +
number_male_90to104 + number_male_105to119) number_modeltotal_stmatt,
((number_ge150_newshell + number_ge135_oldshell) + number_recruit_prib +
number_male_105to119 + number_male_120to134) number_modeltotal_prib,
number_male_total,
number_female_immature,number_female_mature,
(number_female_immature + number_female_mature) number_female_total,
number_unsexed_total
from bk_number_sizegroup_temp
order by cruise,vessel,haul,gis_station;	

-- Similarly, by weight				

drop table bk_weight_mt_sizegroup_temp;

create table bk_weight_mt_sizegroup_temp as
select hauljoin, vessel, cruise, haul, gis_station, species_code,
	   (sum(CASE
	   			WHEN  size1 between 0 and 89.0
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_le89,
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
	   			WHEN  size1 between 90.0 and 104.9
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_90to104,				  
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
	   			WHEN  size1 between 90.0 and 250
			    THEN mt_male_size1
				ELSE 0
				END))  mt_male_ge90,				
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
	   			WHEN  size1 between 120 and 250
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
	   			WHEN  size1 between 120.0 and 133.9
				AND shell_condition in (1,2)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_recruit_stmatt,	
	   (sum(CASE
	   			WHEN  size1 between 134.0 and 250
				AND shell_condition in (1,2)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_ge134_newshell,	
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 250
				AND shell_condition in (1,2)
			    THEN mt_male_size1
				ELSE 0
				END))  mt_ge135_newshell,				        
	   (sum(CASE
	   			WHEN  size1 between 135.0 and 149
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
     sum(mt_female_size1_immature) mt_female_immature,
     sum(mt_female_size1_mature) mt_female_mature											
	   from bk_weight_mt_size1
         where species_code = 69323
       group by hauljoin,
	            vessel,
				cruise,
				haul,
				gis_station,
				species_code;
				
drop table bk_weight_mt_sizegroup;

create table bk_weight_mt_sizegroup as
select hauljoin,vessel,cruise,haul,gis_station,species_code,
mt_male_le89,mt_male_le104,mt_male_le109,mt_male_le119,
mt_male_le119_newshell,mt_male_90to104,mt_male_105to119,mt_male_110to134,
mt_male_120to134,mt_male_ge90,mt_male_ge105,mt_male_ge120,
mt_male_ge120_newshell,mt_male_ge120_oldshell,
mt_male_ge135,mt_recruit_stmatt,mt_recruit_prib,
(mt_ge134_newshell + mt_male_ge120_oldshell) mt_pr_stmatt,
(mt_ge150_newshell + mt_ge135_oldshell) mt_pr_prib,
((mt_ge134_newshell + mt_male_ge120_oldshell) + mt_recruit_stmatt +
mt_male_90to104 + mt_male_105to119) mt_modeltotal_stmatt,
((mt_ge150_newshell + mt_ge135_oldshell) + mt_recruit_prib + 
mt_male_105to119 + mt_male_120to134) mt_modeltotal_prib,
mt_male_total,
mt_female_immature,mt_female_mature,
(mt_female_immature + mt_female_mature) mt_female_total
from bk_weight_mt_sizegroup_temp
order by cruise,vessel,haul,gis_station;
					

-- This section combines the haul and catch data, including
-- those haul/size groups where there was no catch.				

drop table bk_num_sizegroup_union;

create table bk_num_sizegroup_union as
select h.hauljoin,h.vessel,h.cruise,h.haul,h.gis_station,survey_year,
nvl(species_code,69323) species_code,
nvl(number_male_le89,0) number_male_le89,
nvl(number_male_le104,0) number_male_le104,
nvl(number_male_le109,0) number_male_le109,
nvl(number_male_le119,0) number_male_le119,
nvl(number_male_le119_newshell,0) number_male_le119_newshell,
nvl(number_male_90to104,0) number_male_90to104,
nvl(number_male_105to119,0) number_male_105to119,
nvl(number_male_110to134,0) number_male_110to134,
nvl(number_male_120to134,0) number_male_120to134,
nvl(number_male_ge90,0) number_male_ge90,
nvl(number_male_ge105,0) number_male_ge105,
nvl(number_male_ge120,0) number_male_ge120,
nvl(number_male_ge120_newshell,0) number_male_ge120_newshell,
nvl(number_male_ge120_oldshell,0) number_male_ge120_oldshell,
nvl(number_male_ge135,0) number_male_ge135,
nvl(number_recruit_stmatt,0) number_recruit_stmatt,
nvl(number_recruit_prib,0) number_recruit_prib,
nvl(number_pr_stmatt,0) number_pr_stmatt,
nvl(number_pr_prib,0) number_pr_prib,
nvl(number_modeltotal_stmatt,0) number_modeltotal_stmatt,
nvl(number_modeltotal_prib,0) number_modeltotal_prib,
nvl(number_male_total,0) number_male_total,
nvl(number_female_immature,0) number_female_immature,
nvl(number_female_mature,0) number_female_mature,
nvl(number_female_total,0) number_female_total,
nvl(number_unsexed_total,0) number_unsexed_total
from haul_newtimeseries_noretow h full outer join bk_number_sizegroup c
on h.hauljoin = c.hauljoin;

--  Similarly, by weight.

drop table bk_wgt_sizegroup_union;

create table bk_wgt_sizegroup_union as
select h.hauljoin,h.vessel,h.cruise,h.haul,h.gis_station,survey_year,
nvl(species_code,69323) species_code,
nvl(mt_male_le89,0) mt_male_le89,
nvl(mt_male_le104,0) mt_male_le104,
nvl(mt_male_le109,0) mt_male_le109,
nvl(mt_male_le119,0) mt_male_le119,
nvl(mt_male_le119_newshell,0) mt_male_le119_newshell,
nvl(mt_male_90to104,0) mt_male_90to104,
nvl(mt_male_105to119,0) mt_male_105to119,
nvl(mt_male_110to134,0) mt_male_110to134,
nvl(mt_male_120to134,0) mt_male_120to134,
nvl(mt_male_ge90,0) mt_male_ge90,
nvl(mt_male_ge105,0) mt_male_ge105,
nvl(mt_male_ge120,0) mt_male_ge120,
nvl(mt_male_ge120_newshell,0) mt_male_ge120_newshell,
nvl(mt_male_ge120_oldshell,0) mt_male_ge120_oldshell,
nvl(mt_male_ge135,0) mt_male_ge135,
nvl(mt_recruit_stmatt,0) mt_recruit_stmatt,
nvl(mt_recruit_prib,0) mt_recruit_prib,
nvl(mt_pr_stmatt,0) mt_pr_stmatt,
nvl(mt_pr_prib,0) mt_pr_prib,
nvl(mt_modeltotal_stmatt,0) mt_modeltotal_stmatt,
nvl(mt_modeltotal_prib,0) mt_modeltotal_prib,
nvl(mt_male_total,0) mt_male_total,
nvl(mt_female_immature,0) mt_female_immature,
nvl(mt_female_mature,0) mt_female_mature,
nvl(mt_female_total,0) mt_female_total
from haul_newtimeseries_noretow h full outer join bk_weight_mt_sizegroup c
on h.hauljoin = c.hauljoin;

-- This section calculates cpue for each haul.
-- If a station contains multiple tows, cpue
-- is calculated for each of the tows, not averaged for the station.
-- A value, even if 0 for no catch, is output for every size group,
-- every haul.  CPUE is calculated as number of crabs per square
-- nautical mile towed; area swept is the distance fished multiplied
-- by the actual (measured) net width.

drop table bk_cpuenum_sizegroup;

create table bk_cpuenum_sizegroup as
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,gear_temperature,
c.gis_station,c.survey_year,c.species_code,
(number_male_le89 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le89,
(number_male_le104 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le104,
(number_male_le109 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le109,
(number_male_le119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le119,
(number_male_le119_newshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_le119_newshell,
(number_male_90to104 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_90to104,
(number_male_105to119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_105to119,
(number_male_110to134 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_110to134,
(number_male_120to134 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_120to134,
(number_male_ge90 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge90,
(number_male_ge105 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge105,
(number_male_ge120 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge120,
(number_male_ge120_newshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge120_newshell,
(number_male_ge120_oldshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge120_oldshell,
(number_male_ge135 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_ge135,
(number_recruit_stmatt / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_recruit_stmatt,
(number_recruit_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_recruit_prib,
(number_pr_stmatt / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_pr_stmatt,
(number_pr_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_pr_prib,
(number_modeltotal_stmatt / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_modeltotal_stmatt,
(number_modeltotal_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_modeltotal_prib,
(number_male_total / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuenum_total,
(number_female_immature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_immature,
(number_female_mature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_mature,
(number_female_total / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuenum_total,
(number_unsexed_total / (((net_width/1000) * distance_fished) * 0.29155335)) unsexed_cpuenum_total
from bk_num_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin
and haul_type <> 17;


-----

drop table smbkc_cpuenum_sizegroup;

create table smbkc_cpuenum_sizegroup as select * from 
bk_cpuenum_sizegroup
where gis_station in
(select station_id from
strata_bkc_newtimeseries
where strata_bkc_newtimeseries.district like 'St. Matt%'
and strata_bkc_newtimeseries.survey_year = bk_cpuenum_sizegroup.survey_year);



drop table smbkc_male_cpuenum_weight_temp;

create table smbkc_male_cpuenum_weight_temp as 
select survey_year,cruise,
sum(gear_temperature * male_cpuenum_total)/sum(male_cpuenum_total) male_weighted_gear_temp
from smbkc_cpuenum_sizegroup
group by survey_year,cruise;




