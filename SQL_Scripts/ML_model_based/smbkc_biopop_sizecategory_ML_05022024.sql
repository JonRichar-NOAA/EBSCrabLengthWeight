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
    WHEN mid_latitude > 58.65 
        AND a.cruise >= 197501
        AND a.shell_condition in(1,2)
        THEN ((p.log10_a_ns * (power(size1,p.b))) * number_male_size1)
		WHEN a.cruise >= 197501
        AND a.shell_condition in(0,3,4,5)
        THEN ((p.log10_a_os * (power(size1,p.b))) * number_male_size1)
        ELSE 0
		END)  wgt_male_size1
from bk_number_size1_male a, haul_newtimeseries_noretow b, smbkc_male_sw_temp_params p
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
			THEN
				 ((0.02065 * (power(size1,2.27))) * number_female_size1)
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
number_unsexed_total,
(number_male_total + number_female_immature + number_female_mature+number_unsexed_total) number_total
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
(mt_female_immature + mt_female_mature) mt_female_total,
(mt_male_total + mt_female_immature + mt_female_mature) mt_total
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
nvl(number_unsexed_total,0) number_unsexed_total,
nvl(number_total,0) number_total
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
nvl(mt_female_total,0) mt_female_total,
nvl(mt_total,0) mt_total
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
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,
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
(number_unsexed_total / (((net_width/1000) * distance_fished) * 0.29155335)) unsexed_cpuenum_total,
(number_total / (((net_width/1000) * distance_fished) * 0.29155335)) cpuenum_total
from bk_num_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin
and haul_type <> 17;


-- This section calculates cpue by weight for each haul.
-- If a station contains multiple tows, cpue is calculated 
-- for each of the tows, not averaged for the station.
-- A value, even if 0 for no catch, is output for every size group,
-- every haul.  CPUE is calculated as weight of crabs (already converted to metric tons) per square
-- nautical mile towed; area swept is the distance fished multiplied
-- by the actual (measured) net width.

drop table bk_cpuewgt_sizegroup;

create table bk_cpuewgt_sizegroup as
select c.hauljoin,c.vessel,c.cruise,c.haul,mid_latitude,mid_longitude,
c.gis_station,c.survey_year,c.species_code,
(mt_male_le89 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le89,
(mt_male_le104 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le104,
(mt_male_le109 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le109,
(mt_male_le119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le119,
(mt_male_le119_newshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_le119_newshell,
(mt_male_90to104 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_90to104,
(mt_male_105to119 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_105to119,
(mt_male_110to134 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_110to134,
(mt_male_120to134 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_120to134,
(mt_male_ge90 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge90,
(mt_male_ge105 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge105,
(mt_male_ge120 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge120,
(mt_male_ge120_newshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge120_newshell,
(mt_male_ge120_oldshell / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge120_oldshell,
(mt_male_ge135 / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_ge135,
(mt_recruit_stmatt / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_recruit_stmatt,
(mt_recruit_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_recruit_prib,
(mt_pr_stmatt / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_pr_stmatt,
(mt_pr_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_pr_prib,
(mt_modeltotal_stmatt / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_modeltotal_stmatt,
(mt_modeltotal_prib / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_modeltotal_prib,
(mt_male_total / (((net_width/1000) * distance_fished) * 0.29155335)) male_cpuewgt_total,
(mt_female_immature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_immature,
(mt_female_mature / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_mature,
(mt_female_total / (((net_width/1000) * distance_fished) * 0.29155335)) female_cpuewgt_total,
(mt_total / (((net_width/1000) * distance_fished) * 0.29155335)) cpuewgt_total
from bk_wgt_sizegroup_union c, haul_newtimeseries_noretow h
where c.hauljoin = h.hauljoin
and haul_type <> 17;


-- Strata/stock level calculations

-- This section calculates the mean cpue by stratum

drop table bk_meancpuenum_sizegroup;

create table bk_meancpuenum_sizegroup as
select c.survey_year,district,
AVG (male_cpuenum_le89) meancpuenum_male_le89,
AVG (male_cpuenum_le104) meancpuenum_male_le104,
AVG (male_cpuenum_le109) meancpuenum_male_le109,
AVG (male_cpuenum_le119) meancpuenum_male_le119,
AVG (male_cpuenum_le119_newshell) meancpuenum_male_le119_new,
AVG (male_cpuenum_90to104) meancpuenum_male_90to104,
AVG (male_cpuenum_105to119) meancpuenum_male_105to119,
AVG (male_cpuenum_110to134) meancpuenum_male_110to134,
AVG (male_cpuenum_120to134) meancpuenum_male_120to134,
AVG (male_cpuenum_ge90) meancpuenum_male_ge90,
AVG (male_cpuenum_ge105) meancpuenum_male_ge105,
AVG (male_cpuenum_ge120) meancpuenum_male_ge120,
AVG (male_cpuenum_ge120_newshell) meancpuenum_male_ge120_new,
AVG (male_cpuenum_ge120_oldshell) meancpuenum_male_ge120_old,
AVG (male_cpuenum_ge135) meancpuenum_male_ge135,
AVG (cpuenum_recruit_stmatt) meancpuenum_recruit_stmatt,
AVG (cpuenum_recruit_prib) meancpuenum_recruit_prib,
AVG (cpuenum_pr_stmatt) meancpuenum_pr_stmatt,
AVG (cpuenum_pr_prib) meancpuenum_pr_prib,
AVG (cpuenum_modeltotal_stmatt) meancpuenum_modeltotal_stmatt,
AVG (cpuenum_modeltotal_prib) meancpuenum_modeltotal_prib,
AVG (male_cpuenum_total) meancpuenum_male_total,
AVG (female_cpuenum_immature) meancpuenum_female_immature,
AVG (female_cpuenum_mature) meancpuenum_female_mature,
AVG (female_cpuenum_total) meancpuenum_female_total,
AVG (unsexed_cpuenum_total) meancpuenum_unsexed_total,
AVG (male_cpuenum_total + female_cpuenum_total + unsexed_cpuenum_total) meancpuenum_gtotal
from bk_cpuenum_sizegroup c, strata_bkc_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;

drop table bk_meancpuewgt_sizegroup;

create table bk_meancpuewgt_sizegroup as
select c.survey_year,district,
AVG (male_cpuewgt_le89) meancpuewgt_male_le89,
AVG (male_cpuewgt_le104) meancpuewgt_male_le104,
AVG (male_cpuewgt_le109) meancpuewgt_male_le109,
AVG (male_cpuewgt_le119) meancpuewgt_male_le119,
AVG (male_cpuewgt_le119_newshell) meancpuewgt_male_le119_new,
AVG (male_cpuewgt_90to104) meancpuewgt_male_90to104,
AVG (male_cpuewgt_105to119) meancpuewgt_male_105to119,
AVG (male_cpuewgt_110to134) meancpuewgt_male_110to134,
AVG (male_cpuewgt_120to134) meancpuewgt_male_120to134,
AVG (male_cpuewgt_ge90) meancpuewgt_male_ge90,
AVG (male_cpuewgt_ge105) meancpuewgt_male_ge105,
AVG (male_cpuewgt_ge120) meancpuewgt_male_ge120,
AVG (male_cpuewgt_ge120_newshell) meancpuewgt_male_ge120_new,
AVG (male_cpuewgt_ge120_oldshell) meancpuewgt_male_ge120_old,
AVG (male_cpuewgt_ge135) meancpuewgt_male_ge135,
AVG (cpuewgt_recruit_stmatt) meancpuewgt_recruit_stmatt,
AVG (cpuewgt_recruit_prib) meancpuewgt_recruit_prib,
AVG (cpuewgt_pr_stmatt) meancpuewgt_pr_stmatt,
AVG (cpuewgt_pr_prib) meancpuewgt_pr_prib,
AVG (cpuewgt_modeltotal_stmatt) meancpuewgt_modeltotal_stmatt,
AVG (cpuewgt_modeltotal_prib) meancpuewgt_modeltotal_prib,
AVG (male_cpuewgt_total) meancpuewgt_male_total,
AVG (female_cpuewgt_immature) meancpuewgt_female_immature,
AVG (female_cpuewgt_mature) meancpuewgt_female_mature,
AVG (female_cpuewgt_total) meancpuewgt_female_total,
AVG (male_cpuewgt_total + female_cpuewgt_total) meancpuewgt_gtotal
from bk_cpuewgt_sizegroup c, strata_bkc_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;


drop table bk_popbystratum_sizegroup;

create table bk_popbystratum_sizegroup as
select distinct c.survey_year,stratum,c.district,
 (meancpuenum_male_le89 * total_area) pop_male_le89,
 (meancpuenum_male_le104 * total_area) pop_male_le104,
 (meancpuenum_male_le109 * total_area) pop_male_le109,
 (meancpuenum_male_le119 * total_area) pop_male_le119,
 (meancpuenum_male_le119_new * total_area) pop_male_le119_newshell,
 (meancpuenum_male_90to104 * total_area) pop_male_90to104,
 (meancpuenum_male_105to119 * total_area) pop_male_105to119,
 (meancpuenum_male_110to134 * total_area) pop_male_110to134,
 (meancpuenum_male_120to134 * total_area) pop_male_120to134,
 (meancpuenum_male_ge90 * total_area) pop_male_ge90,
 (meancpuenum_male_ge105 * total_area) pop_male_ge105,
 (meancpuenum_male_ge120 * total_area) pop_male_ge120,
 (meancpuenum_male_ge120_new * total_area) pop_male_ge120_newshell,
 (meancpuenum_male_ge120_old * total_area) pop_male_ge120_oldshell,
 (meancpuenum_male_ge135 * total_area) pop_male_ge135,
 (meancpuenum_recruit_stmatt * total_area) pop_recruit_stmatt,
 (meancpuenum_recruit_prib * total_area) pop_recruit_prib,
 (meancpuenum_pr_stmatt * total_area) pop_pr_stmatt,
 (meancpuenum_pr_prib * total_area) pop_pr_prib,
 (meancpuenum_modeltotal_stmatt * total_area) pop_modeltotal_stmatt,
 (meancpuenum_modeltotal_prib * total_area) pop_modeltotal_prib,
 (meancpuenum_male_total * total_area) pop_male_total,
 (meancpuenum_female_immature * total_area) pop_female_immature,
 (meancpuenum_female_mature * total_area) pop_female_mature,
 (meancpuenum_female_total * total_area) pop_female_total,
 (meancpuenum_unsexed_total * total_area) pop_unsexed_total,
 (meancpuenum_gtotal * total_area) pop_gtotal
from bk_meancpuenum_sizegroup c, strata_bkc_newtimeseries s
where c.district = s.district
and c.survey_year = s.survey_year
order by survey_year,district;

drop table bk_biobystratum_sizegroup;

create table bk_biobystratum_sizegroup as
select distinct c.survey_year,stratum,c.district,
(meancpuewgt_male_le89 * total_area) bio_male_le89,
(meancpuewgt_male_le104 * total_area) bio_male_le104,
(meancpuewgt_male_le109 * total_area) bio_male_le109,
(meancpuewgt_male_le119 * total_area) bio_male_le119,
(meancpuewgt_male_le119_new * total_area) bio_male_le119_newshell,
(meancpuewgt_male_90to104 * total_area) bio_male_90to104,
(meancpuewgt_male_105to119 * total_area) bio_male_105to119,
(meancpuewgt_male_110to134 * total_area) bio_male_110to134,
(meancpuewgt_male_120to134 * total_area) bio_male_120to134,
(meancpuewgt_male_ge90 * total_area) bio_male_ge90,
(meancpuewgt_male_ge105 * total_area) bio_male_ge105,
(meancpuewgt_male_ge120 * total_area) bio_male_ge120,
(meancpuewgt_male_ge120_new * total_area) bio_male_ge120_newshell,
(meancpuewgt_male_ge120_old * total_area) bio_male_ge120_oldshell,
(meancpuewgt_male_ge135 * total_area) bio_male_ge135,
(meancpuewgt_recruit_stmatt * total_area) bio_recruit_stmatt,
(meancpuewgt_recruit_prib * total_area) bio_recruit_prib,
(meancpuewgt_pr_stmatt * total_area) bio_pr_stmatt,
(meancpuewgt_pr_prib * total_area) bio_pr_prib,
(meancpuewgt_modeltotal_stmatt * total_area) bio_modeltotal_stmatt,
(meancpuewgt_modeltotal_prib * total_area) bio_modeltotal_prib,
(meancpuewgt_male_total * total_area) bio_male_total,
(meancpuewgt_female_immature * total_area) bio_female_immature,
(meancpuewgt_female_mature * total_area) bio_female_mature,
(meancpuewgt_female_total * total_area) bio_female_total,
(meancpuewgt_gtotal * total_area) bio_gtotal
from bk_meancpuewgt_sizegroup c, strata_bkc_newtimeseries s
where c.district = s.district
and c.survey_year = s.survey_year
order by survey_year,district;


drop table bk_varcpuenum_sizegroup;

create table bk_varcpuenum_sizegroup as
select c.survey_year,district,
VARIANCE (male_cpuenum_le89) varcpuenum_male_le89,
VARIANCE (male_cpuenum_le104) varcpuenum_male_le104,
VARIANCE (male_cpuenum_le109) varcpuenum_male_le109,
VARIANCE (male_cpuenum_le119) varcpuenum_male_le119,
VARIANCE (male_cpuenum_le119_newshell) varcpuenum_male_le119_newshell,
VARIANCE (male_cpuenum_90to104) varcpuenum_male_90to104,
VARIANCE (male_cpuenum_105to119) varcpuenum_male_105to119,
VARIANCE (male_cpuenum_110to134) varcpuenum_male_110to134,
VARIANCE (male_cpuenum_120to134) varcpuenum_male_120to134,
VARIANCE (male_cpuenum_ge90) varcpuenum_male_ge90,
VARIANCE (male_cpuenum_ge105) varcpuenum_male_ge105,
VARIANCE (male_cpuenum_ge120) varcpuenum_male_ge120,
VARIANCE (male_cpuenum_ge120_newshell) varcpuenum_male_ge120_newshell,
VARIANCE (male_cpuenum_ge120_oldshell) varcpuenum_male_ge120_oldshell,
VARIANCE (male_cpuenum_ge135) varcpuenum_male_ge135,
VARIANCE (cpuenum_recruit_stmatt) varcpuenum_recruit_stmatt,
VARIANCE (cpuenum_recruit_prib) varcpuenum_recruit_prib,
VARIANCE (cpuenum_pr_stmatt) varcpuenum_pr_stmatt,
VARIANCE (cpuenum_pr_prib) varcpuenum_pr_prib,
VARIANCE (cpuenum_modeltotal_stmatt) varcpuenum_modeltotal_stmatt,
VARIANCE (cpuenum_modeltotal_prib) varcpuenum_modeltotal_prib,
VARIANCE (male_cpuenum_total) varcpuenum_male_total,
VARIANCE (female_cpuenum_immature) varcpuenum_female_immature,
VARIANCE (female_cpuenum_mature) varcpuenum_female_mature,
VARIANCE (female_cpuenum_total) varcpuenum_female_total,
VARIANCE (unsexed_cpuenum_total) varcpuenum_unsexed_total,
VARIANCE (male_cpuenum_total + female_cpuenum_total + unsexed_cpuenum_total) varcpuenum_gtotal
from bk_cpuenum_sizegroup c, strata_bkc_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;

drop table bk_varcpuewgt_sizegroup;

create table bk_varcpuewgt_sizegroup as
select c.survey_year,district,
VARIANCE (male_cpuewgt_le89) varcpuewgt_male_le89,
VARIANCE (male_cpuewgt_le104) varcpuewgt_male_le104,
VARIANCE (male_cpuewgt_le109) varcpuewgt_male_le109,
VARIANCE (male_cpuewgt_le119) varcpuewgt_male_le119,
VARIANCE (male_cpuewgt_le119_newshell) varcpuewgt_male_le119_newshell,
VARIANCE (male_cpuewgt_90to104) varcpuewgt_male_90to104,
VARIANCE (male_cpuewgt_105to119) varcpuewgt_male_105to119,
VARIANCE (male_cpuewgt_110to134) varcpuewgt_male_110to134,
VARIANCE (male_cpuewgt_120to134) varcpuewgt_male_120to134,
VARIANCE (male_cpuewgt_ge90) varcpuewgt_male_ge90,
VARIANCE (male_cpuewgt_ge105) varcpuewgt_male_ge105,
VARIANCE (male_cpuewgt_ge120) varcpuewgt_male_ge120,
VARIANCE (male_cpuewgt_ge120_newshell) varcpuewgt_male_ge120_newshell,
VARIANCE (male_cpuewgt_ge120_oldshell) varcpuewgt_male_ge120_oldshell,
VARIANCE (male_cpuewgt_ge135) varcpuewgt_male_ge135,
VARIANCE (cpuewgt_recruit_stmatt) varcpuewgt_recruit_stmatt,
VARIANCE (cpuewgt_recruit_prib) varcpuewgt_recruit_prib,
VARIANCE (cpuewgt_pr_stmatt) varcpuewgt_pr_stmatt,
VARIANCE (cpuewgt_pr_prib) varcpuewgt_pr_prib,
VARIANCE (cpuewgt_modeltotal_stmatt) varcpuewgt_modeltotal_stmatt,
VARIANCE (cpuewgt_modeltotal_prib) varcpuewgt_modeltotal_prib,
VARIANCE (male_cpuewgt_total) varcpuewgt_male_total,
VARIANCE (female_cpuewgt_immature) varcpuewgt_female_immature,
VARIANCE (female_cpuewgt_mature) varcpuewgt_female_mature,
VARIANCE (female_cpuewgt_total) varcpuewgt_female_total,
VARIANCE (male_cpuewgt_total + female_cpuewgt_total) varcpuewgt_gtotal
from bk_cpuewgt_sizegroup c, strata_bkc_newtimeseries s
where c.gis_station = s.station_id
and c.survey_year = s.survey_year
group by c.survey_year,district;

-- Need to know the number of hauls by stratum for variance calculations

drop table bk_haulcount;

create table bk_haulcount as
select count(hauljoin)number_tows, h.survey_year, district
from haul_newtimeseries_noretow h, strata_bkc_newtimeseries s
where h.gis_station = s.station_id
and h.survey_year = s.survey_year
and haul_type <> 17
group by h.survey_year, district;

drop table bk_variancepop_sizegroup;

create table bk_variancepop_sizegroup as
select distinct c.survey_year,stratum,c.district,
((varcpuenum_male_le89 * (power(total_area,2)))/number_tows) varpop_male_le89,
((varcpuenum_male_le104 * (power(total_area,2)))/number_tows) varpop_male_le104,
((varcpuenum_male_le109 * (power(total_area,2)))/number_tows) varpop_male_le109,
((varcpuenum_male_le119 * (power(total_area,2)))/number_tows) varpop_male_le119,
((varcpuenum_male_le119_newshell * (power(total_area,2)))/number_tows) varpop_male_le119_newshell,
((varcpuenum_male_90to104 * (power(total_area,2)))/number_tows) varpop_male_90to104,
((varcpuenum_male_105to119 * (power(total_area,2)))/number_tows) varpop_male_105to119,
((varcpuenum_male_110to134 * (power(total_area,2)))/number_tows) varpop_male_110to134,
((varcpuenum_male_120to134 * (power(total_area,2)))/number_tows) varpop_male_120to134,
((varcpuenum_male_ge90 * (power(total_area,2)))/number_tows) varpop_male_ge90,
((varcpuenum_male_ge105 * (power(total_area,2)))/number_tows) varpop_male_ge105,
((varcpuenum_male_ge120 * (power(total_area,2)))/number_tows) varpop_male_ge120,
((varcpuenum_male_ge120_newshell * (power(total_area,2)))/number_tows) varpop_male_ge120_newshell,
((varcpuenum_male_ge120_oldshell * (power(total_area,2)))/number_tows) varpop_male_ge120_oldshell,
((varcpuenum_male_ge135 * (power(total_area,2)))/number_tows) varpop_male_ge135,
((varcpuenum_recruit_stmatt * (power(total_area,2)))/number_tows) varpop_recruit_stmatt,
((varcpuenum_recruit_prib * (power(total_area,2)))/number_tows) varpop_recruit_prib,
((varcpuenum_pr_stmatt * (power(total_area,2)))/number_tows) varpop_pr_stmatt,
((varcpuenum_pr_prib * (power(total_area,2)))/number_tows) varpop_pr_prib,
((varcpuenum_modeltotal_stmatt * (power(total_area,2)))/number_tows) varpop_modeltotal_stmatt,
((varcpuenum_modeltotal_prib * (power(total_area,2)))/number_tows) varpop_modeltotal_prib,
((varcpuenum_male_total * (power(total_area,2)))/number_tows) varpop_male_total,
((varcpuenum_female_immature * (power(total_area,2)))/number_tows) varpop_female_immature,
((varcpuenum_female_mature * (power(total_area,2)))/number_tows) varpop_female_mature,
((varcpuenum_female_total * (power(total_area,2)))/number_tows) varpop_female_total,
((varcpuenum_unsexed_total * (power(total_area,2)))/number_tows) varpop_unsexed_total,
((varcpuenum_gtotal * (power(total_area,2)))/number_tows) varpop_gtotal
from strata_bkc_newtimeseries s, bk_varcpuenum_sizegroup c, bk_haulcount n
where c.district = s.district
and c.district = n.district
and c.survey_year = s.survey_year
and c.survey_year = n.survey_year
order by c.survey_year,stratum;

drop table bk_variancebio_sizegroup;

create table bk_variancebio_sizegroup as
select distinct c.survey_year,stratum,c.district,
((varcpuewgt_male_le89 * (power(total_area,2)))/number_tows) varbio_male_le89,
((varcpuewgt_male_le104 * (power(total_area,2)))/number_tows) varbio_male_le104,
((varcpuewgt_male_le109 * (power(total_area,2)))/number_tows) varbio_male_le109,
((varcpuewgt_male_le119 * (power(total_area,2)))/number_tows) varbio_male_le119,
((varcpuewgt_male_le119_newshell * (power(total_area,2)))/number_tows) varbio_male_le119_newshell,
((varcpuewgt_male_90to104 * (power(total_area,2)))/number_tows) varbio_male_90to104,
((varcpuewgt_male_105to119 * (power(total_area,2)))/number_tows) varbio_male_105to119,
((varcpuewgt_male_110to134 * (power(total_area,2)))/number_tows) varbio_male_110to134,
((varcpuewgt_male_120to134 * (power(total_area,2)))/number_tows) varbio_male_120to134,
((varcpuewgt_male_ge90 * (power(total_area,2)))/number_tows) varbio_male_ge90,
((varcpuewgt_male_ge105 * (power(total_area,2)))/number_tows) varbio_male_ge105,
((varcpuewgt_male_ge120 * (power(total_area,2)))/number_tows) varbio_male_ge120,
((varcpuewgt_male_ge120_newshell * (power(total_area,2)))/number_tows) varbio_male_ge120_newshell,
((varcpuewgt_male_ge120_oldshell * (power(total_area,2)))/number_tows) varbio_male_ge120_oldshell,
((varcpuewgt_male_ge135 * (power(total_area,2)))/number_tows) varbio_male_ge135,
((varcpuewgt_recruit_stmatt * (power(total_area,2)))/number_tows) varbio_recruit_stmatt,
((varcpuewgt_recruit_prib * (power(total_area,2)))/number_tows) varbio_recruit_prib,
((varcpuewgt_pr_stmatt * (power(total_area,2)))/number_tows) varbio_pr_stmatt,
((varcpuewgt_pr_prib * (power(total_area,2)))/number_tows) varbio_pr_prib,
((varcpuewgt_modeltotal_stmatt * (power(total_area,2)))/number_tows) varbio_modeltotal_stmatt,
((varcpuewgt_modeltotal_prib * (power(total_area,2)))/number_tows) varbio_modeltotal_prib,
((varcpuewgt_male_total * (power(total_area,2)))/number_tows) varbio_male_total,
((varcpuewgt_female_immature * (power(total_area,2)))/number_tows) varbio_female_immature,
((varcpuewgt_female_mature * (power(total_area,2)))/number_tows) varbio_female_mature,
((varcpuewgt_female_total * (power(total_area,2)))/number_tows) varbio_female_total,
((varcpuewgt_gtotal * (power(total_area,2)))/number_tows) varbio_gtotal
from strata_bkc_newtimeseries s, bk_varcpuewgt_sizegroup c, bk_haulcount n
where c.district = s.district
and c.district = n.district
and c.survey_year = s.survey_year
and c.survey_year = n.survey_year
order by c.survey_year,stratum;

-- Calculations by stock or district from this point on
-- Pribilof bkc this section

drop table pribbk_pop_sizegroup;

create table pribbk_pop_sizegroup as
select survey_year,
sum(pop_male_le104) sum_pop_male_le104,
sum(pop_male_le109) sum_pop_male_le109,
sum(pop_male_le119) sum_pop_male_le119,
sum(pop_male_le119_newshell) sum_pop_male_le119_newshell,
sum(pop_male_105to119) sum_pop_male_105to119,
sum(pop_male_110to134) sum_pop_male_110to134,
sum(pop_male_120to134) sum_pop_male_120to134,
sum(pop_male_ge105) sum_pop_male_ge105,
sum(pop_male_ge120) sum_pop_male_ge120,
sum(pop_male_ge120_newshell) sum_pop_male_ge120_newshell,
sum(pop_male_ge120_oldshell) sum_pop_male_ge120_oldshell,
sum(pop_male_ge135) sum_pop_male_ge135,
sum(pop_recruit_prib) sum_pop_recruit_prib,
sum(pop_pr_prib) sum_pop_pr_prib,
sum(pop_modeltotal_prib) sum_pop_modeltotal_prib,
sum(pop_male_total) sum_pop_male_total,
sum(pop_female_immature) sum_pop_female_immature,
sum(pop_female_mature) sum_pop_female_mature,
sum(pop_female_total) sum_pop_female_total,
sum(pop_unsexed_total) sum_pop_unsexed_total,
sum(pop_gtotal) sum_pop_gtotal
from bk_popbystratum_sizegroup
where district like 'Prib%'
group by survey_year
order by survey_year;

drop table pribbk_bio_sizegroup;

create table pribbk_bio_sizegroup as
select survey_year,
sum(bio_male_le104) sum_bio_male_le104,
sum(bio_male_le109) sum_bio_male_le109,
sum(bio_male_le119) sum_bio_male_le119,
sum(bio_male_le119_newshell) sum_bio_male_le119_newshell,
sum(bio_male_105to119) sum_bio_male_105to119,
sum(bio_male_110to134) sum_bio_male_110to134,
sum(bio_male_120to134) sum_bio_male_120to134,
sum(bio_male_ge105) sum_bio_male_ge105,
sum(bio_male_ge120) sum_bio_male_ge120,
sum(bio_male_ge120_newshell) sum_bio_male_ge120_newshell,
sum(bio_male_ge120_oldshell) sum_bio_male_ge120_oldshell,
sum(bio_male_ge135) sum_bio_male_ge135,
sum(bio_recruit_prib) sum_bio_recruit_prib,
sum(bio_pr_prib) sum_bio_pr_prib,
sum(bio_modeltotal_prib) sum_bio_modeltotal_prib,
sum(bio_male_total) sum_bio_male_total,
sum(bio_female_immature) sum_bio_female_immature,
sum(bio_female_mature) sum_bio_female_mature,
sum(bio_female_total) sum_bio_female_total,
sum(bio_gtotal) sum_bio_gtotal
from bk_biobystratum_sizegroup
where district like 'Prib%'
group by survey_year
order by survey_year;


drop table pribbk_varpop_sizegroup_sum;

create table pribbk_varpop_sizegroup_sum as
select distinct survey_year,
sum(varpop_male_le104) sum_varpop_male_le104,
sum(varpop_male_le109) sum_varpop_male_le109,
sum(varpop_male_le119) sum_varpop_male_le119,
sum(varpop_male_le119_newshell) sum_varpop_male_le119_newshell,
sum(varpop_male_105to119) sum_varpop_male_105to119,
sum(varpop_male_110to134) sum_varpop_male_110to134,
sum(varpop_male_120to134) sum_varpop_male_120to134,
sum(varpop_male_ge105) sum_varpop_male_ge105,
sum(varpop_male_ge120) sum_varpop_male_ge120,
sum(varpop_male_ge120_newshell) sum_varpop_male_ge120_newshell,
sum(varpop_male_ge120_oldshell) sum_varpop_male_ge120_oldshell,
sum(varpop_male_ge135) sum_varpop_male_ge135,
sum(varpop_recruit_prib) sum_varpop_recruit_prib,
sum(varpop_pr_prib) sum_varpop_pr_prib,
sum(varpop_modeltotal_prib) sum_varpop_modeltotal_prib,
sum(varpop_male_total) sum_varpop_male_total,
sum(varpop_female_immature) sum_varpop_female_immature,
sum(varpop_female_mature) sum_varpop_female_mature,
sum(varpop_female_total) sum_varpop_female_total,
sum(varpop_unsexed_total) sum_varpop_unsexed_total,
sum(varpop_gtotal) sum_varpop_gtotal
from bk_variancepop_sizegroup
where district like 'Prib%'
group by survey_year
order by survey_year;

drop table pribbk_varbio_sizegroup_sum;

create table pribbk_varbio_sizegroup_sum as
select distinct survey_year,
sum(varbio_male_le104) sum_varbio_male_le104,
sum(varbio_male_le109) sum_varbio_male_le109,
sum(varbio_male_le119) sum_varbio_male_le119,
sum(varbio_male_le119_newshell) sum_varbio_male_le119_newshell,
sum(varbio_male_105to119) sum_varbio_male_105to119,
sum(varbio_male_110to134) sum_varbio_male_110to134,
sum(varbio_male_120to134) sum_varbio_male_120to134,
sum(varbio_male_ge105) sum_varbio_male_ge105,
sum(varbio_male_ge120) sum_varbio_male_ge120,
sum(varbio_male_ge120_newshell) sum_varbio_male_ge120_newshell,
sum(varbio_male_ge120_oldshell) sum_varbio_male_ge120_oldshell,
sum(varbio_male_ge135) sum_varbio_male_ge135,
sum(varbio_recruit_prib) sum_varbio_recruit_prib,
sum(varbio_pr_prib) sum_varbio_pr_prib,
sum(varbio_modeltotal_prib) sum_varbio_modeltotal_prib,
sum(varbio_male_total) sum_varbio_male_total,
sum(varbio_female_immature) sum_varbio_female_immature,
sum(varbio_female_mature) sum_varbio_female_mature,
sum(varbio_female_total) sum_varbio_female_total,
sum(varbio_gtotal) sum_varbio_gtotal
from bk_variancebio_sizegroup
where district like 'Prib%'
group by survey_year
order by survey_year;

drop table pribbk_pop_sizegroup_cv;

create table pribbk_pop_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_pop_male_le104 <> 0
	 then ((sqrt(sum_varpop_male_le104))/sum_pop_male_le104)
	 else 0
	 end) cv_pop_male_le104,
(CASE
	 when sum_pop_male_le109 <> 0
	 then ((sqrt(sum_varpop_male_le109))/sum_pop_male_le109)
	 else 0
	 end) cv_pop_male_le109,	 
(CASE
	 when sum_pop_male_le119 <> 0
	 then ((sqrt(sum_varpop_male_le119))/sum_pop_male_le119)
	 else 0
	 end) cv_pop_male_le119,
(CASE
	 when sum_pop_male_le119_newshell <> 0
	 then ((sqrt(sum_varpop_male_le119_newshell))/sum_pop_male_le119_newshell)
	 else 0
	 end) cv_pop_male_le119_newshell,   
(CASE
	 when sum_pop_male_105to119 <> 0
	 then ((sqrt(sum_varpop_male_105to119))/sum_pop_male_105to119)
	 else 0
	 end) cv_pop_male_105to119,
(CASE
	 when sum_pop_male_110to134 <> 0
	 then ((sqrt(sum_varpop_male_110to134))/sum_pop_male_110to134)
	 else 0
	 end) cv_pop_male_110to134,	 
(CASE
	 when sum_pop_male_120to134 <> 0
	 then ((sqrt(sum_varpop_male_120to134))/sum_pop_male_120to134)
	 else 0
	 end) cv_pop_male_120to134,	 
(CASE
	 when sum_pop_male_ge105 <> 0
	 then ((sqrt(sum_varpop_male_ge105))/sum_pop_male_ge105)
	 else 0
	 end) cv_pop_male_ge105,	
(CASE
	 when sum_pop_male_ge120 <> 0
	 then ((sqrt(sum_varpop_male_ge120))/sum_pop_male_ge120)
	 else 0
	 end) cv_pop_male_ge120,
(CASE
	 when sum_pop_male_ge120_newshell <> 0
	 then ((sqrt(sum_varpop_male_ge120_newshell))/sum_pop_male_ge120_newshell)
	 else 0
	 end) cv_pop_male_ge120_newshell,
(CASE
	 when sum_pop_male_ge120_oldshell <> 0
	 then ((sqrt(sum_varpop_male_ge120_oldshell))/sum_pop_male_ge120_oldshell)
	 else 0
	 end) cv_pop_male_ge120_oldshell,   
(CASE
	 when sum_pop_male_ge135 <> 0
	 then ((sqrt(sum_varpop_male_ge135))/sum_pop_male_ge135)
	 else 0
	 end) cv_pop_male_ge135,
(CASE
	 when sum_pop_recruit_prib <> 0
	 then ((sqrt(sum_varpop_recruit_prib))/sum_pop_recruit_prib)
	 else 0
	 end) cv_pop_recruit_prib,
(CASE
	 when sum_pop_pr_prib <> 0
	 then ((sqrt(sum_varpop_pr_prib))/sum_pop_pr_prib)
	 else 0
	 end) cv_pop_pr_prib,	 	 
(CASE
	 when sum_pop_modeltotal_prib <> 0
	 then ((sqrt(sum_varpop_modeltotal_prib))/sum_pop_modeltotal_prib)
	 else 0
	 end) cv_pop_modeltotal_prib,
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
from pribbk_varpop_sizegroup_sum a, pribbk_pop_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;	 	 	 

drop table pribbk_bio_sizegroup_cv;

create table pribbk_bio_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_bio_male_le104 <> 0
	 then ((sqrt(sum_varbio_male_le104))/sum_bio_male_le104)
	 else 0
	 end) cv_bio_male_le104,
(CASE
	 when sum_bio_male_le109 <> 0
	 then ((sqrt(sum_varbio_male_le109))/sum_bio_male_le109)
	 else 0
	 end) cv_bio_male_le109,
(CASE
	 when sum_bio_male_le119 <> 0
	 then ((sqrt(sum_varbio_male_le119))/sum_bio_male_le119)
	 else 0
	 end) cv_bio_male_le119,
(CASE
	 when sum_bio_male_le119_newshell <> 0
	 then ((sqrt(sum_varbio_male_le119_newshell))/sum_bio_male_le119_newshell)
	 else 0
	 end) cv_bio_male_le119_newshell,   
(CASE
	 when sum_bio_male_105to119 <> 0
	 then ((sqrt(sum_varbio_male_105to119))/sum_bio_male_105to119)
	 else 0
	 end) cv_bio_male_105to119,
(CASE
	 when sum_bio_male_110to134 <> 0
	 then ((sqrt(sum_varbio_male_110to134))/sum_bio_male_110to134)
	 else 0
	 end) cv_bio_male_110to134,	 
(CASE
	 when sum_bio_male_120to134 <> 0
	 then ((sqrt(sum_varbio_male_120to134))/sum_bio_male_120to134)
	 else 0
	 end) cv_bio_male_120to134,	 
(CASE
	 when sum_bio_male_ge105 <> 0
	 then ((sqrt(sum_varbio_male_ge105))/sum_bio_male_ge105)
	 else 0
	 end) cv_bio_male_ge105,	
(CASE
	 when sum_bio_male_ge120 <> 0
	 then ((sqrt(sum_varbio_male_ge120))/sum_bio_male_ge120)
	 else 0
	 end) cv_bio_male_ge120,	 
(CASE
	 when sum_bio_male_ge120_newshell <> 0
	 then ((sqrt(sum_varbio_male_ge120_newshell))/sum_bio_male_ge120_newshell)
	 else 0
	 end) cv_bio_male_ge120_newshell,	 
(CASE
	 when sum_bio_male_ge120_oldshell <> 0
	 then ((sqrt(sum_varbio_male_ge120_oldshell))/sum_bio_male_ge120_oldshell)
	 else 0
	 end) cv_bio_male_ge120_oldshell,	   
(CASE
	 when sum_bio_male_ge135 <> 0
	 then ((sqrt(sum_varbio_male_ge135))/sum_bio_male_ge135)
	 else 0
	 end) cv_bio_male_ge135,
(CASE
	 when sum_bio_recruit_prib <> 0
	 then ((sqrt(sum_varbio_recruit_prib))/sum_bio_recruit_prib)
	 else 0
	 end) cv_bio_recruit_prib,
(CASE
	 when sum_bio_pr_prib <> 0
	 then ((sqrt(sum_varbio_pr_prib))/sum_bio_pr_prib)
	 else 0
	 end) cv_bio_pr_prib,	 	 
(CASE
	 when sum_bio_modeltotal_prib <> 0
	 then ((sqrt(sum_varbio_modeltotal_prib))/sum_bio_modeltotal_prib)
	 else 0
	 end) cv_bio_modeltotal_prib,
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
from pribbk_varbio_sizegroup_sum a, pribbk_bio_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;	 


-- CI calcs

create or replace view pribbk_sizegroup_stderr_pop as
select distinct survey_year,
(sqrt(sum_varpop_male_le104)) stderr_pop_male_le104,
(sqrt(sum_varpop_male_le109)) stderr_pop_male_le109,
(sqrt(sum_varpop_male_le119)) stderr_pop_male_le119,
(sqrt(sum_varpop_male_le119_newshell)) stderr_pop_male_le119_newshell,
(sqrt(sum_varpop_male_105to119)) stderr_pop_male_105to119,
(sqrt(sum_varpop_male_110to134)) stderr_pop_male_110to134,
(sqrt(sum_varpop_male_120to134)) stderr_pop_male_120to134,
(sqrt(sum_varpop_male_ge105)) stderr_pop_male_ge105,
(sqrt(sum_varpop_male_ge120)) stderr_pop_male_ge120,
(sqrt(sum_varpop_male_ge120_newshell)) stderr_pop_male_ge120_newshell,
(sqrt(sum_varpop_male_ge120_oldshell)) stderr_pop_male_ge120_oldshell,
(sqrt(sum_varpop_male_ge135)) stderr_pop_male_ge135,
(sqrt(sum_varpop_recruit_prib)) stderr_pop_recruit_prib,
(sqrt(sum_varpop_pr_prib)) stderr_pop_pr_prib,
(sqrt(sum_varpop_modeltotal_prib)) stderr_pop_modeltotal_prib,
(sqrt(sum_varpop_male_total)) stderr_pop_male_total,
(sqrt(sum_varpop_female_immature)) stderr_pop_female_immature,
(sqrt(sum_varpop_female_mature)) stderr_pop_female_mature,
(sqrt(sum_varpop_female_total)) stderr_pop_female_total,
(sqrt(sum_varpop_unsexed_total)) stderr_pop_unsexed_total,
(sqrt(sum_varpop_gtotal)) stderr_pop_gtotal
from pribbk_varpop_sizegroup_sum;

create or replace view pribbk_sizegroup_stderr_bio as
select distinct survey_year,
(sqrt(sum_varbio_male_le104)) stderr_bio_male_le104,
(sqrt(sum_varbio_male_le109)) stderr_bio_male_le109,
(sqrt(sum_varbio_male_le119)) stderr_bio_male_le119,
(sqrt(sum_varbio_male_le119_newshell)) stderr_bio_male_le119_newshell,
(sqrt(sum_varbio_male_105to119)) stderr_bio_male_105to119,
(sqrt(sum_varbio_male_110to134)) stderr_bio_male_110to134,
(sqrt(sum_varbio_male_120to134)) stderr_bio_male_120to134,
(sqrt(sum_varbio_male_ge105)) stderr_bio_male_ge105,
(sqrt(sum_varbio_male_ge120)) stderr_bio_male_ge120,
(sqrt(sum_varbio_male_ge120_newshell)) stderr_bio_male_ge120_newshell,
(sqrt(sum_varbio_male_ge120_oldshell)) stderr_bio_male_ge120_oldshell,
(sqrt(sum_varbio_male_ge135)) stderr_bio_male_ge135,
(sqrt(sum_varbio_recruit_prib)) stderr_bio_recruit_prib,
(sqrt(sum_varbio_pr_prib)) stderr_bio_pr_prib,
(sqrt(sum_varbio_modeltotal_prib)) stderr_bio_modeltotal_prib,
(sqrt(sum_varbio_male_total)) stderr_bio_male_total,
(sqrt(sum_varbio_female_immature)) stderr_bio_female_immature,
(sqrt(sum_varbio_female_mature)) stderr_bio_female_mature,
(sqrt(sum_varbio_female_total)) stderr_bio_female_total,
(sqrt(sum_varbio_gtotal)) stderr_bio_gtotal
from pribbk_varbio_sizegroup_sum;

drop table pribbk_sizegroup_ci_pop;

create table pribbk_sizegroup_ci_pop as
select distinct survey_year,
(1.96 * stderr_pop_male_le104) ci_pop_male_le104,
(1.96 * stderr_pop_male_le109) ci_pop_male_le109,
(1.96 * stderr_pop_male_le119) ci_pop_male_le119,
(1.96 * stderr_pop_male_le119_newshell) ci_pop_male_le119_newshell,
(1.96 * stderr_pop_male_105to119) ci_pop_male_105to119,
(1.96 * stderr_pop_male_110to134) ci_pop_male_110to134,
(1.96 * stderr_pop_male_120to134) ci_pop_male_120to134,
(1.96 * stderr_pop_male_ge105) ci_pop_male_ge105,
(1.96 * stderr_pop_male_ge120) ci_pop_male_ge120,
(1.96 * stderr_pop_male_ge120_newshell) ci_pop_male_ge120_newshell,
(1.96 * stderr_pop_male_ge120_oldshell) ci_pop_male_ge120_oldshell,
(1.96 * stderr_pop_male_ge135) ci_pop_male_ge135,
(1.96 * stderr_pop_recruit_prib) ci_pop_recruit_prib,
(1.96 * stderr_pop_pr_prib) ci_pop_pr_prib,
(1.96 * stderr_pop_modeltotal_prib) ci_pop_modeltotal_prib,
(1.96 * stderr_pop_male_total) ci_pop_male_total,
(1.96 * stderr_pop_female_immature) ci_pop_female_immature,
(1.96 * stderr_pop_female_mature) ci_pop_female_mature,
(1.96 * stderr_pop_female_total) ci_pop_female_total,
(1.96 * stderr_pop_unsexed_total) ci_pop_unsexed_total,
(1.96 * stderr_pop_gtotal) ci_pop_gtotal
from pribbk_sizegroup_stderr_pop;

drop table pribbk_sizegroup_ci_bio;

create table pribbk_sizegroup_ci_bio as
select distinct survey_year,
((1.96 * stderr_bio_male_le104)) ci_bio_male_le104,
((1.96 * stderr_bio_male_le109)) ci_bio_male_le109,
((1.96 * stderr_bio_male_le119)) ci_bio_male_le119,
((1.96 * stderr_bio_male_le119_newshell)) ci_bio_male_le119_newshell,
((1.96 * stderr_bio_male_105to119)) ci_bio_male_105to119,
((1.96 * stderr_bio_male_110to134)) ci_bio_male_110to134,
((1.96 * stderr_bio_male_120to134)) ci_bio_male_120to134,
((1.96 * stderr_bio_male_ge105)) ci_bio_male_ge105,
((1.96 * stderr_bio_male_ge120)) ci_bio_male_ge120,
((1.96 * stderr_bio_male_ge120_newshell)) ci_bio_male_ge120_newshell,
((1.96 * stderr_bio_male_ge120_oldshell)) ci_bio_male_ge120_oldshell,
((1.96 * stderr_bio_male_ge135)) ci_bio_male_ge135,
((1.96 * stderr_bio_recruit_prib)) ci_bio_recruit_prib,
((1.96 * stderr_bio_pr_prib)) ci_bio_pr_prib,
((1.96 * stderr_bio_modeltotal_prib)) ci_bio_modeltotal_prib,
((1.96 * stderr_bio_male_total)) ci_bio_male_total,
((1.96 * stderr_bio_female_immature)) ci_bio_female_immature,
((1.96 * stderr_bio_female_mature)) ci_bio_female_mature,
((1.96 * stderr_bio_female_total)) ci_bio_female_total,
((1.96 * stderr_bio_gtotal)) ci_bio_gtotal
from pribbk_sizegroup_stderr_bio;

 	 	 	 	  
 	 
-- St Matt bkc this section

drop table stmattbk_pop_sizegroup;

create table stmattbk_pop_sizegroup as
select survey_year,
sum(pop_male_le89) sum_pop_male_le89,
sum(pop_male_le104) sum_pop_male_le104,
sum(pop_male_90to104) sum_pop_male_90to104,
sum(pop_male_105to119) sum_pop_male_105to119,
sum(pop_male_ge90) sum_pop_male_ge90,
sum(pop_male_ge105) sum_pop_male_ge105,
sum(pop_male_ge120) sum_pop_male_ge120,
sum(pop_recruit_stmatt) sum_pop_recruit_stmatt,
sum(pop_pr_stmatt) sum_pop_pr_stmatt,
sum(pop_modeltotal_stmatt) sum_pop_modeltotal_stmatt,
sum(pop_male_total) sum_pop_male_total,
sum(pop_female_immature) sum_pop_female_immature,
sum(pop_female_mature) sum_pop_female_mature,
sum(pop_female_total) sum_pop_female_total,
sum(pop_unsexed_total) sum_pop_unsexed_total,
sum(pop_gtotal) sum_pop_gtotal
from bk_popbystratum_sizegroup
where district like 'St. Matt%'
group by survey_year
order by survey_year;

drop table stmattbk_bio_sizegroup;

create table stmattbk_bio_sizegroup as
select survey_year,
sum(bio_male_le89) sum_bio_male_le89,
sum(bio_male_le104) sum_bio_male_le104,
sum(bio_male_90to104) sum_bio_male_90to104,
sum(bio_male_105to119) sum_bio_male_105to119,
sum(bio_male_ge90) sum_bio_male_ge90,
sum(bio_male_ge105) sum_bio_male_ge105,
sum(bio_male_ge120) sum_bio_male_ge120,
sum(bio_recruit_stmatt) sum_bio_recruit_stmatt,
sum(bio_pr_stmatt) sum_bio_pr_stmatt,
sum(bio_modeltotal_stmatt) sum_bio_modeltotal_stmatt,
sum(bio_male_total) sum_bio_male_total,
sum(bio_female_immature) sum_bio_female_immature,
sum(bio_female_mature) sum_bio_female_mature,
sum(bio_female_total) sum_bio_female_total,
sum(bio_gtotal) sum_bio_gtotal
from bk_biobystratum_sizegroup
where district like 'St. Matt%'
group by survey_year
order by survey_year;


drop table stmattbk_varpop_sizegroup_sum;

create table stmattbk_varpop_sizegroup_sum as
select distinct survey_year,
sum(varpop_male_le89) sum_varpop_male_le89,
sum(varpop_male_le104) sum_varpop_male_le104,
sum(varpop_male_90to104) sum_varpop_male_90to104,
sum(varpop_male_105to119) sum_varpop_male_105to119,
sum(varpop_male_ge90) sum_varpop_male_ge90,
sum(varpop_male_ge105) sum_varpop_male_ge105,
sum(varpop_male_ge120) sum_varpop_male_ge120,
sum(varpop_recruit_stmatt) sum_varpop_recruit_stmatt,
sum(varpop_pr_stmatt) sum_varpop_pr_stmatt,
sum(varpop_modeltotal_stmatt) sum_varpop_modeltotal_stmatt,
sum(varpop_male_total) sum_varpop_male_total,
sum(varpop_female_immature) sum_varpop_female_immature,
sum(varpop_female_mature) sum_varpop_female_mature,
sum(varpop_female_total) sum_varpop_female_total,
sum(varpop_unsexed_total) sum_varpop_unsexed_total,
sum(varpop_gtotal) sum_varpop_gtotal
from bk_variancepop_sizegroup
where district like 'St. Matt%'
group by survey_year
order by survey_year;

drop table stmattbk_varbio_sizegroup_sum;

create table stmattbk_varbio_sizegroup_sum as
select distinct survey_year,
sum(varbio_male_le89) sum_varbio_male_le89,
sum(varbio_male_le104) sum_varbio_male_le104,
sum(varbio_male_90to104) sum_varbio_male_90to104,
sum(varbio_male_105to119) sum_varbio_male_105to119,
sum(varbio_male_ge90) sum_varbio_male_ge90,
sum(varbio_male_ge105) sum_varbio_male_ge105,
sum(varbio_male_ge120) sum_varbio_male_ge120,
sum(varbio_recruit_stmatt) sum_varbio_recruit_stmatt,
sum(varbio_pr_stmatt) sum_varbio_pr_stmatt,
sum(varbio_modeltotal_stmatt) sum_varbio_modeltotal_stmatt,
sum(varbio_male_total) sum_varbio_male_total,
sum(varbio_female_immature) sum_varbio_female_immature,
sum(varbio_female_mature) sum_varbio_female_mature,
sum(varbio_female_total) sum_varbio_female_total,
sum(varbio_gtotal) sum_varbio_gtotal
from bk_variancebio_sizegroup
where district like 'St. Matt%'
group by survey_year
order by survey_year;

drop table stmattbk_pop_sizegroup_cv;

create table stmattbk_pop_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_pop_male_le89 <> 0
	 then ((sqrt(sum_varpop_male_le89))/sum_pop_male_le89)
	 else 0
	 end) cv_pop_male_le89,
(CASE
	 when sum_pop_male_le104 <> 0
	 then ((sqrt(sum_varpop_male_le104))/sum_pop_male_le104)
	 else 0
	 end) cv_pop_male_le104,
(CASE
	 when sum_pop_male_90to104 <> 0
	 then ((sqrt(sum_varpop_male_90to104))/sum_pop_male_90to104)
	 else 0
	 end) cv_pop_male_90to104,
(CASE
	 when sum_pop_male_105to119 <> 0
	 then ((sqrt(sum_varpop_male_105to119))/sum_pop_male_105to119)
	 else 0
	 end) cv_pop_male_105to119,
(CASE
	 when sum_pop_male_ge90 <> 0
	 then ((sqrt(sum_varpop_male_ge90))/sum_pop_male_ge90)
	 else 0
	 end) cv_pop_male_ge90,	 
(CASE
	 when sum_pop_male_ge105 <> 0
	 then ((sqrt(sum_varpop_male_ge105))/sum_pop_male_ge105)
	 else 0
	 end) cv_pop_male_ge105,	 	 
(CASE
	 when sum_pop_male_ge120 <> 0
	 then ((sqrt(sum_varpop_male_ge120))/sum_pop_male_ge120)
	 else 0
	 end) cv_pop_male_ge120,
(CASE
	 when sum_pop_recruit_stmatt <> 0
	 then ((sqrt(sum_varpop_recruit_stmatt))/sum_pop_recruit_stmatt)
	 else 0
	 end) cv_pop_recruit_stmatt,
(CASE
	 when sum_pop_pr_stmatt <> 0
	 then ((sqrt(sum_varpop_pr_stmatt))/sum_pop_pr_stmatt)
	 else 0
	 end) cv_pop_pr_stmatt,	 
(CASE
	 when sum_pop_modeltotal_stmatt <> 0
	 then ((sqrt(sum_varpop_modeltotal_stmatt))/sum_pop_modeltotal_stmatt)
	 else 0
	 end) cv_pop_modeltotal_stmatt,
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
from stmattbk_varpop_sizegroup_sum a, stmattbk_pop_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;	 	 	 

drop table stmattbk_bio_sizegroup_cv;

create table stmattbk_bio_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_bio_male_le89 <> 0
	 then ((sqrt(sum_varbio_male_le89))/sum_bio_male_le89)
	 else 0
	 end) cv_bio_male_le89,
(CASE
	 when sum_bio_male_le104 <> 0
	 then ((sqrt(sum_varbio_male_le104))/sum_bio_male_le104)
	 else 0
	 end) cv_bio_male_le104,
(CASE
	 when sum_bio_male_90to104 <> 0
	 then ((sqrt(sum_varbio_male_90to104))/sum_bio_male_90to104)
	 else 0
	 end) cv_bio_male_90to104,
(CASE
	 when sum_bio_male_105to119 <> 0
	 then ((sqrt(sum_varbio_male_105to119))/sum_bio_male_105to119)
	 else 0
	 end) cv_bio_male_105to119,
(CASE
	 when sum_bio_male_ge90 <> 0
	 then ((sqrt(sum_varbio_male_ge90))/sum_bio_male_ge90)
	 else 0
	 end) cv_bio_male_ge90,	 
(CASE
	 when sum_bio_male_ge105 <> 0
	 then ((sqrt(sum_varbio_male_ge105))/sum_bio_male_ge105)
	 else 0
	 end) cv_bio_male_ge105,	 	 
(CASE
	 when sum_bio_male_ge120 <> 0
	 then ((sqrt(sum_varbio_male_ge120))/sum_bio_male_ge120)
	 else 0
	 end) cv_bio_male_ge120,
(CASE
	 when sum_bio_recruit_stmatt <> 0
	 then ((sqrt(sum_varbio_recruit_stmatt))/sum_bio_recruit_stmatt)
	 else 0
	 end) cv_bio_recruit_stmatt,
(CASE
	 when sum_bio_pr_stmatt <> 0
	 then ((sqrt(sum_varbio_pr_stmatt))/sum_bio_pr_stmatt)
	 else 0
	 end) cv_bio_pr_stmatt,	 
(CASE
	 when sum_bio_modeltotal_stmatt <> 0
	 then ((sqrt(sum_varbio_modeltotal_stmatt))/sum_bio_modeltotal_stmatt)
	 else 0
	 end) cv_bio_modeltotal_stmatt,
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
from stmattbk_varbio_sizegroup_sum a, stmattbk_bio_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;


-- CI calcs

create or replace view stmattbk_sizegroup_stderr_pop as
select distinct survey_year,
(sqrt(sum_varpop_male_le89)) stderr_pop_male_le89,
(sqrt(sum_varpop_male_le104)) stderr_pop_male_le104,
(sqrt(sum_varpop_male_90to104)) stderr_pop_male_90to104,
(sqrt(sum_varpop_male_105to119)) stderr_pop_male_105to119,
(sqrt(sum_varpop_male_ge90)) stderr_pop_male_ge90,
(sqrt(sum_varpop_male_ge105)) stderr_pop_male_ge105,
(sqrt(sum_varpop_male_ge120)) stderr_pop_male_ge120,
(sqrt(sum_varpop_recruit_stmatt)) stderr_pop_recruit_stmatt,
(sqrt(sum_varpop_pr_stmatt)) stderr_pop_pr_stmatt,
(sqrt(sum_varpop_modeltotal_stmatt)) stderr_pop_modeltotal_stmatt,
(sqrt(sum_varpop_male_total)) stderr_pop_male_total,
(sqrt(sum_varpop_female_immature)) stderr_pop_female_immature,
(sqrt(sum_varpop_female_mature)) stderr_pop_female_mature,
(sqrt(sum_varpop_female_total)) stderr_pop_female_total,
(sqrt(sum_varpop_unsexed_total)) stderr_pop_unsexed_total,
(sqrt(sum_varpop_gtotal)) stderr_pop_gtotal
from stmattbk_varpop_sizegroup_sum;

create or replace view stmattbk_sizegroup_stderr_bio as
select distinct survey_year,
(sqrt(sum_varbio_male_le89)) stderr_bio_male_le89,
(sqrt(sum_varbio_male_le104)) stderr_bio_male_le104,
(sqrt(sum_varbio_male_90to104)) stderr_bio_male_90to104,
(sqrt(sum_varbio_male_105to119)) stderr_bio_male_105to119,
(sqrt(sum_varbio_male_ge90)) stderr_bio_male_ge90,
(sqrt(sum_varbio_male_ge105)) stderr_bio_male_ge105,
(sqrt(sum_varbio_male_ge120)) stderr_bio_male_ge120,
(sqrt(sum_varbio_recruit_stmatt)) stderr_bio_recruit_stmatt,
(sqrt(sum_varbio_pr_stmatt)) stderr_bio_pr_stmatt,
(sqrt(sum_varbio_modeltotal_stmatt)) stderr_bio_modeltotal_stmatt,
(sqrt(sum_varbio_male_total)) stderr_bio_male_total,
(sqrt(sum_varbio_female_immature)) stderr_bio_female_immature,
(sqrt(sum_varbio_female_mature)) stderr_bio_female_mature,
(sqrt(sum_varbio_female_total)) stderr_bio_female_total,
(sqrt(sum_varbio_gtotal)) stderr_bio_gtotal
from stmattbk_varbio_sizegroup_sum;


drop table stmattbk_sizegroup_ci_pop;

create table stmattbk_sizegroup_ci_pop as
select distinct survey_year,
(1.96 * stderr_pop_male_le89) ci_pop_male_le89,
(1.96 * stderr_pop_male_le104) ci_pop_male_le104,
(1.96 * stderr_pop_male_90to104) ci_pop_male_90to104,
(1.96 * stderr_pop_male_105to119) ci_pop_male_105to119,
(1.96 * stderr_pop_male_ge90) ci_pop_male_ge90,
(1.96 * stderr_pop_male_ge105) ci_pop_male_ge105,
(1.96 * stderr_pop_male_ge120) ci_pop_male_ge120,
(1.96 * stderr_pop_recruit_stmatt) ci_pop_recruit_stmatt,
(1.96 * stderr_pop_pr_stmatt) ci_pop_pr_stmatt,
(1.96 * stderr_pop_modeltotal_stmatt) ci_pop_modeltotal_stmatt,
(1.96 * stderr_pop_male_total) ci_pop_male_total,
(1.96 * stderr_pop_female_immature) ci_pop_female_immature,
(1.96 * stderr_pop_female_mature) ci_pop_female_mature,
(1.96 * stderr_pop_female_total) ci_pop_female_total,
(1.96 * stderr_pop_unsexed_total) ci_pop_unsexed_total,
(1.96 * stderr_pop_gtotal) ci_pop_gtotal
from stmattbk_sizegroup_stderr_pop;

drop table stmattbk_sizegroup_ci_bio;

create table stmattbk_sizegroup_ci_bio as
select distinct survey_year,
((1.96 * stderr_bio_male_le89)) ci_bio_male_le89,
((1.96 * stderr_bio_male_le104)) ci_bio_male_le104,
((1.96 * stderr_bio_male_90to104)) ci_bio_male_90to104,
((1.96 * stderr_bio_male_105to119)) ci_bio_male_105to119,
((1.96 * stderr_bio_male_ge90)) ci_bio_male_ge90,
((1.96 * stderr_bio_male_ge105)) ci_bio_male_ge105,
((1.96 * stderr_bio_male_ge120)) ci_bio_male_ge120,
((1.96 * stderr_bio_recruit_stmatt)) ci_bio_recruit_stmatt,
((1.96 * stderr_bio_pr_stmatt)) ci_bio_pr_stmatt,
((1.96 * stderr_bio_modeltotal_stmatt)) ci_bio_modeltotal_stmatt,
((1.96 * stderr_bio_male_total)) ci_bio_male_total,
((1.96 * stderr_bio_female_immature)) ci_bio_female_immature,
((1.96 * stderr_bio_female_mature)) ci_bio_female_mature,
((1.96 * stderr_bio_female_total)) ci_bio_female_total,
((1.96 * stderr_bio_gtotal)) ci_bio_gtotal
from stmattbk_sizegroup_stderr_bio;

-- Unstratified bkc this section

drop table unstratbk_pop_sizegroup;

create table unstratbk_pop_sizegroup as
select survey_year,
sum(pop_male_le89) sum_pop_male_le89,
sum(pop_male_le104) sum_pop_male_le104,
sum(pop_male_90to104) sum_pop_male_90to104,
sum(pop_male_105to119) sum_pop_male_105to119,
sum(pop_male_ge90) sum_pop_male_ge90,
sum(pop_male_ge105) sum_pop_male_ge105,
sum(pop_male_ge120) sum_pop_male_ge120,
sum(pop_recruit_stmatt) sum_pop_recruit_stmatt,
sum(pop_pr_stmatt) sum_pop_pr_stmatt,
sum(pop_modeltotal_stmatt) sum_pop_modeltotal_stmatt,
sum(pop_male_total) sum_pop_male_total,
sum(pop_female_immature) sum_pop_female_immature,
sum(pop_female_mature) sum_pop_female_mature,
sum(pop_female_total) sum_pop_female_total,
sum(pop_unsexed_total) sum_pop_unsexed_total,
sum(pop_gtotal) sum_pop_gtotal
from bk_popbystratum_sizegroup
where district = 'BKC Unstratified'
group by survey_year
order by survey_year;

drop table unstratbk_bio_sizegroup;

create table unstratbk_bio_sizegroup as
select survey_year,
sum(bio_male_le89) sum_bio_male_le89,
sum(bio_male_le104) sum_bio_male_le104,
sum(bio_male_90to104) sum_bio_male_90to104,
sum(bio_male_105to119) sum_bio_male_105to119,
sum(bio_male_ge90) sum_bio_male_ge90,
sum(bio_male_ge105) sum_bio_male_ge105,
sum(bio_male_ge120) sum_bio_male_ge120,
sum(bio_recruit_stmatt) sum_bio_recruit_stmatt,
sum(bio_pr_stmatt) sum_bio_pr_stmatt,
sum(bio_modeltotal_stmatt) sum_bio_modeltotal_stmatt,
sum(bio_male_total) sum_bio_male_total,
sum(bio_female_immature) sum_bio_female_immature,
sum(bio_female_mature) sum_bio_female_mature,
sum(bio_female_total) sum_bio_female_total,
sum(bio_gtotal) sum_bio_gtotal
from bk_biobystratum_sizegroup
where district = 'BKC Unstratified'
group by survey_year
order by survey_year;


drop table unstratbk_varpop_sizegroup_sum;

create table unstratbk_varpop_sizegroup_sum as
select distinct survey_year,
sum(varpop_male_le89) sum_varpop_male_le89,
sum(varpop_male_le104) sum_varpop_male_le104,
sum(varpop_male_90to104) sum_varpop_male_90to104,
sum(varpop_male_105to119) sum_varpop_male_105to119,
sum(varpop_male_ge90) sum_varpop_male_ge90,
sum(varpop_male_ge105) sum_varpop_male_ge105,
sum(varpop_male_ge120) sum_varpop_male_ge120,
sum(varpop_recruit_stmatt) sum_varpop_recruit_stmatt,
sum(varpop_pr_stmatt) sum_varpop_pr_stmatt,
sum(varpop_modeltotal_stmatt) sum_varpop_modeltotal_stmatt,
sum(varpop_male_total) sum_varpop_male_total,
sum(varpop_female_immature) sum_varpop_female_immature,
sum(varpop_female_mature) sum_varpop_female_mature,
sum(varpop_female_total) sum_varpop_female_total,
sum(varpop_unsexed_total) sum_varpop_unsexed_total,
sum(varpop_gtotal) sum_varpop_gtotal
from bk_variancepop_sizegroup
where district = 'BKC Unstratified'
group by survey_year
order by survey_year;

drop table unstratbk_varbio_sizegroup_sum;

create table unstratbk_varbio_sizegroup_sum as
select distinct survey_year,
sum(varbio_male_le89) sum_varbio_male_le89,
sum(varbio_male_le104) sum_varbio_male_le104,
sum(varbio_male_90to104) sum_varbio_male_90to104,
sum(varbio_male_105to119) sum_varbio_male_105to119,
sum(varbio_male_ge90) sum_varbio_male_ge90,
sum(varbio_male_ge105) sum_varbio_male_ge105,
sum(varbio_male_ge120) sum_varbio_male_ge120,
sum(varbio_recruit_stmatt) sum_varbio_recruit_stmatt,
sum(varbio_pr_stmatt) sum_varbio_pr_stmatt,
sum(varbio_modeltotal_stmatt) sum_varbio_modeltotal_stmatt,
sum(varbio_male_total) sum_varbio_male_total,
sum(varbio_female_immature) sum_varbio_female_immature,
sum(varbio_female_mature) sum_varbio_female_mature,
sum(varbio_female_total) sum_varbio_female_total,
sum(varbio_gtotal) sum_varbio_gtotal
from bk_variancebio_sizegroup
where district = 'BKC Unstratified'
group by survey_year
order by survey_year;

drop table unstratbk_pop_sizegroup_cv;

create table unstratbk_pop_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_pop_male_le89 <> 0
	 then ((sqrt(sum_varpop_male_le89))/sum_pop_male_le89)
	 else 0
	 end) cv_pop_male_le89,
(CASE
	 when sum_pop_male_le104 <> 0
	 then ((sqrt(sum_varpop_male_le104))/sum_pop_male_le104)
	 else 0
	 end) cv_pop_male_le104,
(CASE
	 when sum_pop_male_90to104 <> 0
	 then ((sqrt(sum_varpop_male_90to104))/sum_pop_male_90to104)
	 else 0
	 end) cv_pop_male_90to104,
(CASE
	 when sum_pop_male_105to119 <> 0
	 then ((sqrt(sum_varpop_male_105to119))/sum_pop_male_105to119)
	 else 0
	 end) cv_pop_male_105to119,
(CASE
	 when sum_pop_male_ge90 <> 0
	 then ((sqrt(sum_varpop_male_ge90))/sum_pop_male_ge90)
	 else 0
	 end) cv_pop_male_ge90,	 
(CASE
	 when sum_pop_male_ge105 <> 0
	 then ((sqrt(sum_varpop_male_ge105))/sum_pop_male_ge105)
	 else 0
	 end) cv_pop_male_ge105,	 	 
(CASE
	 when sum_pop_male_ge120 <> 0
	 then ((sqrt(sum_varpop_male_ge120))/sum_pop_male_ge120)
	 else 0
	 end) cv_pop_male_ge120,
(CASE
	 when sum_pop_recruit_stmatt <> 0
	 then ((sqrt(sum_varpop_recruit_stmatt))/sum_pop_recruit_stmatt)
	 else 0
	 end) cv_pop_recruit_stmatt,
(CASE
	 when sum_pop_pr_stmatt <> 0
	 then ((sqrt(sum_varpop_pr_stmatt))/sum_pop_pr_stmatt)
	 else 0
	 end) cv_pop_pr_stmatt,	 
(CASE
	 when sum_pop_modeltotal_stmatt <> 0
	 then ((sqrt(sum_varpop_modeltotal_stmatt))/sum_pop_modeltotal_stmatt)
	 else 0
	 end) cv_pop_modeltotal_stmatt,	 
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
from unstratbk_varpop_sizegroup_sum a, unstratbk_pop_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;	 	 	 

drop table unstratbk_bio_sizegroup_cv;

create table unstratbk_bio_sizegroup_cv as
select a.survey_year,
(CASE
	 when sum_bio_male_le89 <> 0
	 then ((sqrt(sum_varbio_male_le89))/sum_bio_male_le89)
	 else 0
	 end) cv_bio_male_le89,
(CASE
	 when sum_bio_male_le104 <> 0
	 then ((sqrt(sum_varbio_male_le104))/sum_bio_male_le104)
	 else 0
	 end) cv_bio_male_le104,
(CASE
	 when sum_bio_male_90to104 <> 0
	 then ((sqrt(sum_varbio_male_90to104))/sum_bio_male_90to104)
	 else 0
	 end) cv_bio_male_90to104,
(CASE
	 when sum_bio_male_105to119 <> 0
	 then ((sqrt(sum_varbio_male_105to119))/sum_bio_male_105to119)
	 else 0
	 end) cv_bio_male_105to119,
(CASE
	 when sum_bio_male_ge90 <> 0
	 then ((sqrt(sum_varbio_male_ge90))/sum_bio_male_ge90)
	 else 0
	 end) cv_bio_male_ge90,	 
(CASE
	 when sum_bio_male_ge105 <> 0
	 then ((sqrt(sum_varbio_male_ge105))/sum_bio_male_ge105)
	 else 0
	 end) cv_bio_male_ge105,	 	 
(CASE
	 when sum_bio_male_ge120 <> 0
	 then ((sqrt(sum_varbio_male_ge120))/sum_bio_male_ge120)
	 else 0
	 end) cv_bio_male_ge120,
(CASE
	 when sum_bio_recruit_stmatt <> 0
	 then ((sqrt(sum_varbio_recruit_stmatt))/sum_bio_recruit_stmatt)
	 else 0
	 end) cv_bio_recruit_stmatt,
(CASE
	 when sum_bio_pr_stmatt <> 0
	 then ((sqrt(sum_varbio_pr_stmatt))/sum_bio_pr_stmatt)
	 else 0
	 end) cv_bio_pr_stmatt,	
(CASE
	 when sum_bio_modeltotal_stmatt <> 0
	 then ((sqrt(sum_varbio_modeltotal_stmatt))/sum_bio_modeltotal_stmatt)
	 else 0
	 end) cv_bio_modeltotal_stmatt,	  
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
from unstratbk_varbio_sizegroup_sum a, unstratbk_bio_sizegroup b
where a.survey_year = b.survey_year
order by a.survey_year;


-- CI calcs

create or replace view unstratbk_sizegroup_stderr_pop as
select distinct survey_year,
(sqrt(sum_varpop_male_le89)) stderr_pop_male_le89,
(sqrt(sum_varpop_male_le104)) stderr_pop_male_le104,
(sqrt(sum_varpop_male_90to104)) stderr_pop_male_90to104,
(sqrt(sum_varpop_male_105to119)) stderr_pop_male_105to119,
(sqrt(sum_varpop_male_ge90)) stderr_pop_male_ge90,
(sqrt(sum_varpop_male_ge105)) stderr_pop_male_ge105,
(sqrt(sum_varpop_male_ge120)) stderr_pop_male_ge120,
(sqrt(sum_varpop_recruit_stmatt)) stderr_pop_recruit_stmatt,
(sqrt(sum_varpop_pr_stmatt)) stderr_pop_pr_stmatt,
(sqrt(sum_varpop_modeltotal_stmatt)) stderr_pop_modeltotal_stmatt,
(sqrt(sum_varpop_male_total)) stderr_pop_male_total,
(sqrt(sum_varpop_female_immature)) stderr_pop_female_immature,
(sqrt(sum_varpop_female_mature)) stderr_pop_female_mature,
(sqrt(sum_varpop_female_total)) stderr_pop_female_total,
(sqrt(sum_varpop_unsexed_total)) stderr_pop_unsexed_total,
(sqrt(sum_varpop_gtotal)) stderr_pop_gtotal
from unstratbk_varpop_sizegroup_sum;

create or replace view unstratbk_sizegroup_stderr_bio as
select distinct survey_year,
(sqrt(sum_varbio_male_le89)) stderr_bio_male_le89,
(sqrt(sum_varbio_male_le104)) stderr_bio_male_le104,
(sqrt(sum_varbio_male_90to104)) stderr_bio_male_90to104,
(sqrt(sum_varbio_male_105to119)) stderr_bio_male_105to119,
(sqrt(sum_varbio_male_ge90)) stderr_bio_male_ge90,
(sqrt(sum_varbio_male_ge105)) stderr_bio_male_ge105,
(sqrt(sum_varbio_male_ge120)) stderr_bio_male_ge120,
(sqrt(sum_varbio_recruit_stmatt)) stderr_bio_recruit_stmatt,
(sqrt(sum_varbio_pr_stmatt)) stderr_bio_pr_stmatt,
(sqrt(sum_varbio_modeltotal_stmatt)) stderr_bio_modeltotal_stmatt,
(sqrt(sum_varbio_male_total)) stderr_bio_male_total,
(sqrt(sum_varbio_female_immature)) stderr_bio_female_immature,
(sqrt(sum_varbio_female_mature)) stderr_bio_female_mature,
(sqrt(sum_varbio_female_total)) stderr_bio_female_total,
(sqrt(sum_varbio_gtotal)) stderr_bio_gtotal
from unstratbk_varbio_sizegroup_sum;


drop table unstratbk_sizegroup_ci_pop;

create table unstratbk_sizegroup_ci_pop as
select distinct survey_year,
((1.96 * stderr_pop_male_le89)) ci_pop_male_le89,
((1.96 * stderr_pop_male_le104)) ci_pop_male_le104,
((1.96 * stderr_pop_male_90to104)) ci_pop_male_90to104,
((1.96 * stderr_pop_male_105to119)) ci_pop_male_105to119,
((1.96 * stderr_pop_male_ge90)) ci_pop_male_ge90,
((1.96 * stderr_pop_male_ge105)) ci_pop_male_ge105,
((1.96 * stderr_pop_male_ge120)) ci_pop_male_ge120,
((1.96 * stderr_pop_recruit_stmatt)) ci_pop_recruit_stmatt,
((1.96 * stderr_pop_pr_stmatt)) ci_pop_pr_stmatt,
((1.96 * stderr_pop_modeltotal_stmatt)) ci_pop_modeltotal_stmatt,
((1.96 * stderr_pop_male_total)) ci_pop_male_total,
((1.96 * stderr_pop_female_immature)) ci_pop_female_immature,
((1.96 * stderr_pop_female_mature)) ci_pop_female_mature,
((1.96 * stderr_pop_female_total)) ci_pop_female_total,
((1.96 * stderr_pop_unsexed_total)) ci_pop_unsexed_total,
((1.96 * stderr_pop_gtotal)) ci_pop_gtotal
from unstratbk_sizegroup_stderr_pop;

drop table unstratbk_sizegroup_ci_bio;

create table unstratbk_sizegroup_ci_bio as
select distinct survey_year,
((1.96 * stderr_bio_male_le89)) ci_bio_male_le89,
((1.96 * stderr_bio_male_le104)) ci_bio_male_le104,
((1.96 * stderr_bio_male_90to104)) ci_bio_male_90to104,
((1.96 * stderr_bio_male_105to119)) ci_bio_male_105to119,
((1.96 * stderr_bio_male_ge90)) ci_bio_male_ge90,
((1.96 * stderr_bio_male_ge105)) ci_bio_male_ge105,
((1.96 * stderr_bio_male_ge120)) ci_bio_male_ge120,
((1.96 * stderr_bio_recruit_stmatt)) ci_bio_recruit_stmatt,
((1.96 * stderr_bio_pr_stmatt)) ci_bio_pr_stmatt,
((1.96 * stderr_bio_modeltotal_stmatt)) ci_bio_modeltotal_stmatt,
((1.96 * stderr_bio_male_total)) ci_bio_male_total,
((1.96 * stderr_bio_female_immature)) ci_bio_female_immature,
((1.96 * stderr_bio_female_mature)) ci_bio_female_mature,
((1.96 * stderr_bio_female_total)) ci_bio_female_total,
((1.96 * stderr_bio_gtotal)) ci_bio_gtotal
from unstratbk_sizegroup_stderr_bio;


-- Final output for stocks


-- St. Matt bkc

drop table bk_stmatt_bio_matfem_newreg;

create table bk_stmatt_bio_matfem_newreg as
select a.survey_year,
sum_bio_male_le104 biomass_male_le104,cv_bio_male_le104 cv_biomass_male_le104,ci_bio_male_le104 ci_biomass_male_le104,
sum_bio_male_ge105 biomass_male_ge105,cv_bio_male_ge105 cv_biomass_male_ge105,ci_bio_male_ge105 ci_biomass_male_ge105,
sum_bio_male_ge120 biomass_male_ge120,cv_bio_male_ge120 cv_biomass_male_ge120,ci_bio_male_ge120 ci_biomass_male_ge120,
sum_bio_male_le89 biomass_male_le89,cv_bio_male_le89 cv_biomass_male_le89,ci_bio_male_le89 ci_biomass_male_le89,
sum_bio_male_90to104 biomass_male_90to104,cv_bio_male_90to104 cv_biomass_male_90to104,ci_bio_male_90to104 ci_biomass_male_90to104,
sum_bio_male_105to119 biomass_male_105to119,cv_bio_male_105to119 cv_biomass_male_105to119,ci_bio_male_105to119 ci_biomass_male_105to119,
sum_bio_male_ge90 biomass_male_ge90,cv_bio_male_ge90 cv_biomass_male_ge90,ci_bio_male_ge90 ci_biomass_male_ge90,
sum_bio_recruit_stmatt biomass_male_recruit,cv_bio_recruit_stmatt cv_biomass_male_recruit,ci_bio_recruit_stmatt ci_biomass_male_recruit,
sum_bio_pr_stmatt biomass_male_postrecruit,cv_bio_pr_stmatt cv_biomass_male_postrecruit,ci_bio_pr_stmatt ci_biomass_male_postrecruit,
--sum_bio_modeltotal_stmatt biomass_male_modeltotal,cv_bio_modeltotal_stmatt cv_biomass_male_modeltotal,ci_bio_modeltotal_stmatt ci_biomass_male_modeltotal,
sum_bio_male_total biomass_male_total,cv_bio_male_total cv_biomass_male_total,ci_bio_male_total ci_biomass_male_total,
sum_bio_female_immature biomass_female_immature,cv_bio_female_immature cv_biomass_female_immature,ci_bio_female_immature ci_biomass_female_immature,
sum_bio_female_mature biomass_female_mature,cv_bio_female_mature cv_biomass_female_mature,ci_bio_female_mature ci_biomass_female_mature,
sum_bio_female_total biomass_female_total,cv_bio_female_total cv_biomass_female_total,ci_bio_female_total ci_biomass_female_total,
sum_bio_gtotal gtotal_mt,cv_bio_gtotal cv_gtotal_mt,ci_bio_gtotal ci_gtotal_mt
from stmattbk_bio_sizegroup a, stmattbk_sizegroup_ci_bio b,stmattbk_bio_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
order by survey_year;

alter table bk_stmatt_bio_matfem_newregress
add SPECIES_CODE NUMBER;

update bk_stmatt_bio_matfem_newregress
set SPECIES_CODE = 69323;

alter table bk_stmatt_bio_matfem_newregress
add SURVEY_REGION VARCHAR2 (50 BYTE);

update bk_stmatt_bio_matfem_newregress
set SURVEY_REGION = 'St. Matthew Island';

drop table bk_stmatt_pop_matfem_newreg;

create table bk_stmatt_pop_matfem_newreg as
select a.survey_year,
sum_pop_male_le104 num_male_le104,cv_pop_male_le104 cv_num_male_le104,ci_pop_male_le104 ci_num_male_le104,
sum_pop_male_ge105 num_male_ge105,cv_pop_male_ge105 cv_num_male_ge105,ci_pop_male_ge105 ci_num_male_ge105,
sum_pop_male_ge120 num_male_ge120,cv_pop_male_ge120 cv_num_male_ge120,ci_pop_male_ge120 ci_num_male_ge120,
sum_pop_male_le89 num_male_le89,cv_pop_male_le89 cv_num_male_le89,ci_pop_male_le89 ci_num_male_le89,
sum_pop_male_90to104 num_male_90to104,cv_pop_male_90to104 cv_num_male_90to104,ci_pop_male_90to104 ci_num_male_90to104,
sum_pop_male_105to119 num_male_105to119,cv_pop_male_105to119 cv_num_male_105to119,ci_pop_male_105to119 ci_num_male_105to119,
sum_pop_male_ge90 num_male_ge90,cv_pop_male_ge90 cv_num_male_ge90,ci_pop_male_ge90 ci_num_male_ge90,
sum_pop_recruit_stmatt num_male_recruit,cv_pop_recruit_stmatt cv_num_male_recruit,ci_pop_recruit_stmatt ci_num_male_recruit,
sum_pop_pr_stmatt num_male_postrecruit,cv_pop_pr_stmatt cv_num_male_postrecruit,ci_pop_pr_stmatt ci_num_male_postrecruit,
--sum_pop_modeltotal_stmatt num_male_modeltotal,cv_pop_modeltotal_stmatt cv_num_male_modeltotal,ci_pop_modeltotal_stmatt ci_num_male_modeltotal,
sum_pop_male_total num_male_total,cv_pop_male_total cv_num_male_total,ci_pop_male_total ci_num_male_total,
sum_pop_female_immature num_female_immature,cv_pop_female_immature cv_num_female_immature,ci_pop_female_immature ci_num_female_immature,
sum_pop_female_mature num_female_mature,cv_pop_female_mature cv_num_female_mature,ci_pop_female_mature ci_num_female_mature,
sum_pop_female_total num_female_total,cv_pop_female_total cv_num_female_total,ci_pop_female_total ci_num_female_total,
sum_pop_gtotal gtotal,cv_pop_gtotal cv_gtotal,ci_pop_gtotal ci_gtotal
from stmattbk_pop_sizegroup a, stmattbk_sizegroup_ci_pop b, stmattbk_pop_sizegroup_cv c
where a.survey_year = b.survey_year
and a.survey_year = c.survey_year
order by survey_year;

alter table bk_stmatt_pop_matfem_newregress
add SPECIES_CODE NUMBER;

update bk_stmatt_pop_matfem_newregress
set SPECIES_CODE = 69323;

alter table bk_stmatt_pop_matfem_newregress
add SURVEY_REGION VARCHAR2 (50 BYTE);

update bk_stmatt_pop_matfem_newregress
set SURVEY_REGION = 'St. Matthew Island';

--################################################################################
