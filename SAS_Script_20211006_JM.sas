
/*_���Ŀ� ���� �ڷ�� 5���̸� �Ҿ� �ڷ� ��)  BFC_2015*/
/*_child ����-���̽� �ڷῡ�� �Ʊ� �ڷ�  ��)  BFC_2015_child*/
/*_mother ����-���̽� �ڷῡ�� ���� �ڷ� ��)  BFC_2015_mother*/
/*target �� 5�� �̸� �Ҿ� ���� ����� �ڷ��ε� �����ϰ� �ڰ� �ڷῡ�� ���� (��ȿ�� �� �Ϻ� ����)*/
/*target_mc�� ����-���� ���� �ڷ� (���� �ߺ� ����)*/

/*���� �ڷ� ���̺귯��*/
LIBNAME RAW '/userdata01/room039/data_source/user_data';

/*�����ڷ� ����  ���̺귯�� */
LIBNAME A '/userdata01/room039/data_source/work';

/*�۾��� �� ������ */
LIBNAME B '/userdata01/room039/data_source/work2';

/*****************************************************************************************************/
/*****************************************************************************************************/

/*5���̸� �Ҿ� �ڰ��ڷ�*/
DATA A.BFC; 
RETAIN PKEY STD_YYYY BYEAR INDI_DSCM_NO SEX_TYPE AGE SIDO SGG INS INS_G;
SET RAW.BFC_2015-RAW.BFC_2019;
/*�ڰ� ���س⵵-����⵵*/

/*INS: �����; INS_G  ����� ���*/
RENAME CALC_CTRB_FD=INS CALC_CTRB_VTILE_FD=INS_G RVSN_ADDR_CD=SGG ;

AGE=STD_YYYY-BYEAR; 
SIDO=SUBSTR(LEFT(RVSN_ADDR_CD),1,2);

/*����+�������̵� ������ ����Ű */
PKEY=COMPRESS(STD_YYYY) || COMPRESS("-") ||COMPRESS(INDI_DSCM_NO);

DROP CMPR_DSB_GRADE MAIN_DSB_TYPE INDTP_CD GAIBJA_TYPE CNT_ID_HHHI_FD;
RUN;
/*****************************************************************************************************/
/*****************************************************************************************************/

/*����-���̽� �ڷῡ�� ���� �ڰ� */
DATA A.BFC_CHILD; 
RETAIN PKEY STD_YYYY BYEAR INDI_DSCM_NO SEX_TYPE AGE SIDO SGG INS INS_G;
SET RAW.BFC_2015_CHILD RAW.BFC_2016_CHILD RAW.BFC_2017_CHILD RAW.BFC_2018_CHILD RAW.BFC_2019_CHILD; 
/*�ڰ� ���س⵵-����⵵*/

/*INS: �����; INS_G  ����� ���*/
RENAME CALC_CTRB_FD=INS CALC_CTRB_VTILE_FD=INS_G RVSN_ADDR_CD=SGG ;

AGE=STD_YYYY-BYEAR; 
SIDO=SUBSTR(LEFT(RVSN_ADDR_CD),1,2);

/*����+�������̵� ������ ����Ű */
PKEY=COMPRESS(STD_YYYY) || COMPRESS("-") ||COMPRESS(INDI_DSCM_NO);

DROP CMPR_DSB_GRADE MAIN_DSB_TYPE INDTP_CD GAIBJA_TYPE CNT_ID_HHHI_FD;
RUN;
/*****************************************************************************************************/
/*****************************************************************************************************/

/*����-���̽� �ڷῡ�� ���� �ڰ� */
DATA A.BFC_MOTHER; 
RETAIN PKEY STD_YYYY BYEAR INDI_DSCM_NO SEX_TYPE AGE SIDO SGG INS INS_G;
SET RAW.BFC_2015_MOTHER RAW.BFC_2016_MOTHER RAW.BFC_2017_MOTHER RAW.BFC_2018_MOTHER RAW.BFC_2019_MOTHER; 
/*�ڰ� ���س⵵-����⵵*/

/*INS: �����; INS_G  ����� ���*/
RENAME CALC_CTRB_FD=INS CALC_CTRB_VTILE_FD=INS_G RVSN_ADDR_CD=SGG ;

AGE=STD_YYYY-BYEAR; 
SIDO=SUBSTR(LEFT(RVSN_ADDR_CD),1,2);

/*����+�������̵� ������ ����Ű */
PKEY=COMPRESS(STD_YYYY) || COMPRESS("-") ||COMPRESS(INDI_DSCM_NO);

DROP CMPR_DSB_GRADE MAIN_DSB_TYPE INDTP_CD GAIBJA_TYPE CNT_ID_HHHI_FD;
RUN;
/*****************************************************************************************************/
/*****************************************************************************************************/

/*�ڰ� ���� , ���̵�, ���� ���� */
PROC SORT DATA=A.BFC               ; BY INDI_DSCM_NO STD_YYYY;
PROC SORT DATA=A.BFC_CHILD     ; BY INDI_DSCM_NO STD_YYYY;
PROC SORT DATA=A.BFC_MOTHER ; BY INDI_DSCM_NO STD_YYYY;
/*****************************************************************************************************/
/*****************************************************************************************************/

/*����ڷ� ���� */
DATA A.DTH              ; SET RAW.DTH              ; RUN;
DATA A.DTH_CHILD    ; SET RAW.DTH_CHILD    ; RUN;
DATA A.DTH_MOTHER; SET RAW.DTH_MOTHER; RUN;
/*****************************************************************************************************/
/*****************************************************************************************************/
/*���� ���� �ڷ� ���� X --> ���߿� �ʿ��ϸ� ���� */

/*****************************************************************************************************/
/*****************************************************************************************************/
/*5���̸� �Ҿ� ���� ����  */
DATA A.T20_2015; SET RAW.T20_201501-RAW.T20_201512;
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;
DATA A.T20_2016; SET RAW.T20_201601-RAW.T20_201612;
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;
DATA A.T20_2017; SET RAW.T20_201701-RAW.T20_201712;
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;
DATA A.T20_2018; SET RAW.T20_201801-RAW.T20_201812;
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;
DATA A.T20_2019; SET RAW.T20_201901-RAW.T20_201912;
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

/*****************************************************************************************************/
/*****************************************************************************************************/
/*����-���ֿ̽��� ���� ���� ���� */
DATA A.T20_CHILD_2015; SET RAW.T20_201501_CHILD RAW.T20_201502_CHILD RAW.T20_201503_CHILD RAW.T20_201504_CHILD
RAW.T20_201505_CHILD RAW.T20_201506_CHILD RAW.T20_201507_CHILD RAW.T20_201508_CHILD RAW.T20_201509_CHILD 
RAW.T20_201510_CHILD RAW.T20_201511_CHILD RAW.T20_201512_CHILD;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20_CHILD_2016; SET RAW.T20_201601_CHILD RAW.T20_201602_CHILD RAW.T20_201603_CHILD RAW.T20_201604_CHILD
RAW.T20_201605_CHILD RAW.T20_201606_CHILD RAW.T20_201607_CHILD RAW.T20_201608_CHILD RAW.T20_201609_CHILD 
RAW.T20_201610_CHILD RAW.T20_201611_CHILD RAW.T20_201612_CHILD;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20_CHILD_2017; SET RAW.T20_201701_CHILD RAW.T20_201702_CHILD RAW.T20_201703_CHILD RAW.T20_201704_CHILD
RAW.T20_201705_CHILD RAW.T20_201706_CHILD RAW.T20_201707_CHILD RAW.T20_201708_CHILD RAW.T20_201709_CHILD 
RAW.T20_201710_CHILD RAW.T20_201711_CHILD RAW.T20_201712_CHILD;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20_CHILD_2018; SET RAW.T20_201801_CHILD RAW.T20_201802_CHILD RAW.T20_201803_CHILD RAW.T20_201804_CHILD
RAW.T20_201805_CHILD RAW.T20_201806_CHILD RAW.T20_201807_CHILD RAW.T20_201808_CHILD RAW.T20_201809_CHILD 
RAW.T20_201810_CHILD RAW.T20_201811_CHILD RAW.T20_201812_CHILD;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20_CHILD_2019; SET RAW.T20_201901_CHILD RAW.T20_201902_CHILD RAW.T20_201903_CHILD RAW.T20_201904_CHILD
RAW.T20_201905_CHILD RAW.T20_201906_CHILD RAW.T20_201907_CHILD RAW.T20_201908_CHILD RAW.T20_201909_CHILD 
RAW.T20_201910_CHILD RAW.T20_201911_CHILD RAW.T20_201912_CHILD;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

/*****************************************************************************************************/
/*****************************************************************************************************/
/*����-���ֿ̽��� ���� ���� ���� */
DATA A.T20_MOTHER_2015; SET RAW.T20_201501_MOTHER RAW.T20_201502_MOTHER RAW.T20_201503_MOTHER RAW.T20_201504_MOTHER
RAW.T20_201505_MOTHER RAW.T20_201506_MOTHER RAW.T20_201507_MOTHER RAW.T20_201508_MOTHER RAW.T20_201509_MOTHER 
RAW.T20_201510_MOTHER RAW.T20_201511_MOTHER RAW.T20_201512_MOTHER;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20_MOTHER_2016; SET RAW.T20_201601_MOTHER RAW.T20_201602_MOTHER RAW.T20_201603_MOTHER RAW.T20_201604_MOTHER
RAW.T20_201605_MOTHER RAW.T20_201606_MOTHER RAW.T20_201607_MOTHER RAW.T20_201608_MOTHER RAW.T20_201609_MOTHER 
RAW.T20_201610_MOTHER RAW.T20_201611_MOTHER RAW.T20_201612_MOTHER;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20_MOTHER_2017; SET RAW.T20_201701_MOTHER RAW.T20_201702_MOTHER RAW.T20_201703_MOTHER RAW.T20_201704_MOTHER
RAW.T20_201705_MOTHER RAW.T20_201706_MOTHER RAW.T20_201707_MOTHER RAW.T20_201708_MOTHER RAW.T20_201709_MOTHER 
RAW.T20_201710_MOTHER RAW.T20_201711_MOTHER RAW.T20_201712_MOTHER;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20_MOTHER_2018; SET RAW.T20_201801_MOTHER RAW.T20_201802_MOTHER RAW.T20_201803_MOTHER RAW.T20_201804_MOTHER
RAW.T20_201805_MOTHER RAW.T20_201806_MOTHER RAW.T20_201807_MOTHER RAW.T20_201808_MOTHER RAW.T20_201809_MOTHER 
RAW.T20_201810_MOTHER RAW.T20_201811_MOTHER RAW.T20_201812_MOTHER;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

DATA A.T20_MOTHER_2019; SET RAW.T20_201901_MOTHER RAW.T20_201902_MOTHER RAW.T20_201903_MOTHER RAW.T20_201904_MOTHER
RAW.T20_201905_MOTHER RAW.T20_201906_MOTHER RAW.T20_201907_MOTHER RAW.T20_201908_MOTHER RAW.T20_201909_MOTHER 
RAW.T20_201910_MOTHER RAW.T20_201911_MOTHER RAW.T20_201912_MOTHER;

KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 SICK_SYM3 SICK_SYM4 SICK_SYM5 MDCARE_STRT_DT FORM_CD 
VSHSP_DD_CNT MDCARE_DD_CNT FST_HSPTZ_DT; RUN;

/*������ ���᳻�� �ڷ�  merge*/
data a.T20              ; SET A.T20_2015-A.T20_2019; RUN;
data a.T20_CHILD    ; SET A.T20_CHILD_2015-A.T20_CHILD_2019; RUN;
data a.T20_MOTHER; SET A.T20_MOTHER_2015-A.T20_MOTHER_2019; RUN;

/*****************************************************************************************************/
/*****************************************************************************************************/
/*5���̸� �Ҿ� óġ ���� Merge  */
DATA A.T30_2015; SET RAW.T30_201501-RAW.T30_201512; RUN;
DATA A.T30_2016; SET RAW.T30_201601-RAW.T30_201612; RUN;
DATA A.T30_2017; SET RAW.T30_201701-RAW.T30_201712; RUN;
DATA A.T30_2018; SET RAW.T30_201801-RAW.T30_201812; RUN;
DATA A.T30_2019; SET RAW.T30_201901-RAW.T30_201912; RUN;

/*5���̸� �Ҿ� óġ ����  */
DATA A.T30; SET A.T30_2015-A.T30_2019; RUN;

DATA A.T30_CHILD_2015; SET 
RAW.T30_201501_CHILD RAW.T30_201502_CHILD RAW.T30_201503_CHILD RAW.T30_201504_CHILD
RAW.T30_201505_CHILD RAW.T30_201506_CHILD RAW.T30_201507_CHILD RAW.T30_201508_CHILD
RAW.T30_201509_CHILD RAW.T30_201510_CHILD RAW.T30_201511_CHILD RAW.T30_201512_CHILD;
RUN;

DATA A.T30_CHILD_2016; SET 
RAW.T30_201601_CHILD RAW.T30_201602_CHILD RAW.T30_201603_CHILD RAW.T30_201604_CHILD
RAW.T30_201605_CHILD RAW.T30_201606_CHILD RAW.T30_201607_CHILD RAW.T30_201608_CHILD
RAW.T30_201609_CHILD RAW.T30_201610_CHILD RAW.T30_201611_CHILD RAW.T30_201612_CHILD;
RUN;

DATA A.T30_CHILD_2017; SET 
RAW.T30_201701_CHILD RAW.T30_201702_CHILD RAW.T30_201703_CHILD RAW.T30_201704_CHILD
RAW.T30_201705_CHILD RAW.T30_201706_CHILD RAW.T30_201707_CHILD RAW.T30_201708_CHILD
RAW.T30_201709_CHILD RAW.T30_201710_CHILD RAW.T30_201711_CHILD RAW.T30_201712_CHILD;
RUN;

DATA A.T30_CHILD_2018; SET 
RAW.T30_201801_CHILD RAW.T30_201802_CHILD RAW.T30_201803_CHILD RAW.T30_201804_CHILD
RAW.T30_201805_CHILD RAW.T30_201806_CHILD RAW.T30_201807_CHILD RAW.T30_201808_CHILD
RAW.T30_201809_CHILD RAW.T30_201810_CHILD RAW.T30_201811_CHILD RAW.T30_201812_CHILD;
RUN;

DATA A.T30_CHILD_2019; SET 
RAW.T30_201901_CHILD RAW.T30_201902_CHILD RAW.T30_201903_CHILD RAW.T30_201904_CHILD
RAW.T30_201905_CHILD RAW.T30_201906_CHILD RAW.T30_201907_CHILD RAW.T30_201908_CHILD
RAW.T30_201909_CHILD RAW.T30_201910_CHILD RAW.T30_201911_CHILD RAW.T30_201912_CHILD;
RUN;

/*���� ���̽� �� ���� óġ���� MERGE*/
DATA A.T30_CHILD; SET A.T30_CHILD_2015-A.T30_CHILD_2019; RUN;

/*����-���̽� ���� óġ ����  */
DATA A.T30_MOTHER_2015; SET 
RAW.T30_201501_MOTHER RAW.T30_201502_MOTHER RAW.T30_201503_MOTHER RAW.T30_201504_MOTHER
RAW.T30_201505_MOTHER RAW.T30_201506_MOTHER RAW.T30_201507_MOTHER RAW.T30_201508_MOTHER
RAW.T30_201509_MOTHER RAW.T30_201510_MOTHER RAW.T30_201511_MOTHER RAW.T30_201512_MOTHER;
RUN;

DATA A.T30_MOTHER_2016; SET 
RAW.T30_201601_MOTHER RAW.T30_201602_MOTHER RAW.T30_201603_MOTHER RAW.T30_201604_MOTHER
RAW.T30_201605_MOTHER RAW.T30_201606_MOTHER RAW.T30_201607_MOTHER RAW.T30_201608_MOTHER
RAW.T30_201609_MOTHER RAW.T30_201610_MOTHER RAW.T30_201611_MOTHER RAW.T30_201612_MOTHER;
RUN;

DATA A.T30_MOTHER_2017; SET 
RAW.T30_201701_MOTHER RAW.T30_201702_MOTHER RAW.T30_201703_MOTHER RAW.T30_201704_MOTHER
RAW.T30_201705_MOTHER RAW.T30_201706_MOTHER RAW.T30_201707_MOTHER RAW.T30_201708_MOTHER
RAW.T30_201709_MOTHER RAW.T30_201710_MOTHER RAW.T30_201711_MOTHER RAW.T30_201712_MOTHER;
RUN;

DATA A.T30_MOTHER_2018; SET 
RAW.T30_201801_MOTHER RAW.T30_201802_MOTHER RAW.T30_201803_MOTHER RAW.T30_201804_MOTHER
RAW.T30_201805_MOTHER RAW.T30_201806_MOTHER RAW.T30_201807_MOTHER RAW.T30_201808_MOTHER
RAW.T30_201809_MOTHER RAW.T30_201810_MOTHER RAW.T30_201811_MOTHER RAW.T30_201812_MOTHER;
RUN;

DATA A.T30_MOTHER_2019; SET 
RAW.T30_201901_MOTHER RAW.T30_201902_MOTHER RAW.T30_201903_MOTHER RAW.T30_201904_MOTHER
RAW.T30_201905_MOTHER RAW.T30_201906_MOTHER RAW.T30_201907_MOTHER RAW.T30_201908_MOTHER
RAW.T30_201909_MOTHER RAW.T30_201910_MOTHER RAW.T30_201911_MOTHER RAW.T30_201912_MOTHER;
RUN;

/*���� ���̽� �� ���� óġ���� MERGE*/
DATA A.T30_MOTHER; SET A.T30_MOTHER_2015-A.T30_MOTHER_2019; RUN;

/*****************************************************************************************************/
/*****************************************************************************************************/
/*5�� �̸� �Ҿ� ���� ���̵� (����)-> �ڰݿ��� ���� ���̵� ����ϱ�  */
DATA A.TARGET; SET RAW.TARGET; RUN;

/*����-���̽� �����ڷ�  */
DATA A.TARGET_MC; SET RAW.TARGET_MC; RUN;

/*�ڰ� �� */
DATA A.BFC_R; SET A.BFC;
IF AGE<5; RUN;

PROC FREQ DATA=A.BFC_R; TABLES STD_YYYY; RUN;
PROC FREQ DATA=A.BFC_R; TABLES STD_YYYY*SEX_TYPE/LIST; RUN;
PROC FREQ DATA=A.BFC_R; TABLES STD_YYYY*AGE/LIST; RUN;
PROC FREQ DATA=A.BFC_R; TABLES STD_YYYY*SIDO/LIST; RUN;

/*5���̸� �Ҿ� �ڷῡ�� 0���� */
DATA A.BFC_C1; SET A.BFC;
IF AGE=0; RUN;

/*����-���̽� �ڷῡ�� 0�� ���̸�  */
DATA A.BFC_C2; SET A.BFC_CHILD;
IF AGE=0; RUN;

PROC FREQ DATA=A.BFC_C1; TABLES STD_YYYY; RUN;
PROC FREQ DATA=A.BFC_C2; TABLES STD_YYYY; RUN;

/*5���̸� �ڷῡ�� �����Ұ��,  0���� ���� �ڰ� �� ���� */
PROC FREQ DATA=A.BFC_C1; TABLES STD_YYYY; RUN;
PROC FREQ DATA=A.BFC_C1; TABLES STD_YYYY*SEX_TYPE/LIST; RUN;
PROC FREQ DATA=A.BFC_C1; TABLES STD_YYYY*SIDO/LIST; RUN;

/*���� ���ֿ̽��� �����Ұ��,  0���� ���� �ڰ� �� ���� */
PROC FREQ DATA=A.BFC_C2; TABLES STD_YYYY; RUN;
PROC FREQ DATA=A.BFC_C2; TABLES STD_YYYY*SEX_TYPE/LIST; RUN;
PROC FREQ DATA=A.BFC_C2; TABLES STD_YYYY*SIDO/LIST; RUN;


PROC FREQ DATA=A.BFC_MOTHER; TABLES STD_YYYY; RUN;
PROC FREQ DATA=A.BFC_MOTHER; TABLES STD_YYYY*SEX_TYPE/LIST; RUN;
PROC FREQ DATA=A.BFC_MOTHER; TABLES STD_YYYY*AGE/LIST; RUN;
PROC FREQ DATA=A.BFC_MOTHER; TABLES STD_YYYY*SIDO/LIST; RUN;

/**/
proc sql; create table a.z as select * from a.target_mc as a left join a.bfc_c2 as b on a.child_id = b.indi_dscm_no; quit;
PROC FREQ DATA=A.z; TABLES STD_YYYY; RUN;

/*�������̽� �ڷῡ�� ���� ���̵�*/
DATA A.MOTHER; SET A.TARGET_MC;
KEEP MOTHER_ID ; RUN;

/*���� ���̵� ������ */
PROC SORT DATA=A.MOTHER NODUPKEY OUT=A.MOTHER_U ; BY MOTHER_ID; RUN;

/**********************************************************************************************/
/**********************************************************************************************/

PROC FREQ DATA= A.T20; TABLES FORM_CD; RUN;

/*��ȯ �� _C�� 5���̸� ���� 
��ȯ �� _INF�� 5���̸� ���� 
��ȯ��  _M�� ���� */

/*
�������� ��ȯ ���� ��ũ��

OUT: �����ȯ 
S_CODE: ��ȯ ó��
E_CODE: ��ȯ ������
K: 3��  �� OR 4�� ��

*/
%MACRO DISEASE(OUT,S_CODE,E_CODE,K);
DATA B.&OUT._C ; set A.T20;
IF FORM_CD IN("02") AND &S_CODE. <= SUBSTR(SICK_SYM1,1,&K.)<=&E_CODE. THEN K1=2; ELSE K1=0; /*�Կ� �̸鼭 �ֻ� COPD �ڵ�, K1�� �ֻ󺴿� �����ϸ� 2*/
IF FORM_CD IN("02") AND &S_CODE. <= SUBSTR(SICK_SYM2,1,&K.)<=&E_CODE. THEN K2=1; ELSE K2=0; /*�Կ� �̸鼭 �λ� COPD �ڵ�, K2�� �λ󺴿� �����ϸ� 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; 
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4)) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);
RUN; /*ICD_RANK ��+�λ� �߿䵵 ���� ��Ÿ��*/
%MEND;


DATA B.heatrelated_C ; set A.T20;
IF FORM_CD IN("02") AND SUBSTR(SICK_SYM1,1,3) in ("T67","E86") THEN K1=2; ELSE K1=0; /*�Կ� �̸鼭 �ֻ� COPD �ڵ�, K1�� �ֻ󺴿� �����ϸ� 2*/
IF FORM_CD IN("02") AND SUBSTR(SICK_SYM2,1,3) in ("T67","E86") THEN K2=1; ELSE K2=0; /*�Կ� �̸鼭 �λ� COPD �ڵ�, K2�� �λ󺴿� �����ϸ� 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; 
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4)) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);
RUN; /*ICD_RANK ��+�λ� �߿䵵 ���� ��Ÿ��*/


/*---------------------------------------------------------------------------------------------------------------------------------------*/
/*���� �߽�: �¿���ȯ, ȣ�����ȯ, ������ ��ȯ�� �м� */
/*�Ʒ� ��ũ�η� �ϰ� ���������ϱ� */
/*�������*/
%DISEASE(feb,"R56","R56",3);     /*�������*/
%DISEASE(all_rh,"J309","J309",3); /*�˷����⼺��*/
%DISEASE(pne,"J09","J18",3);      /*���*/
%DISEASE(asthma,"J45","J46",3); /*õ��*/
%DISEASE(URI,"J00","J06",3); /*�޼���⵵����*/
%DISEASE(ALRI,"J20","J22",3); /*�޼��ϱ⵵����*/
%DISEASE(acute_bronch1,"J40","J46",3); /*�޼��������*/
%DISEASE(acute_bronch2,"J219","J219",4); /*�޼����������*/
%DISEASE(RSV,"B974","B974",4); /*�޼����������*/
%DISEASE(om,"H65","H67",3);    /*���̿�*/
%DISEASE(intest,"A00","A09",3); /*�尨����ȯ*/
%DISEASE(rota,"A080","A080",4); /*�޼����������*/
%DISEASE(camp,"A045","A045",4); /*į�ʷι��ͼ� ����*/
%DISEASE(HFMD,"B084","B084",4); /*HFMD ��������*/
%DISEASE(mumps,"B269","B269",4); /*���Ÿ�*/
%DISEASE(cardi,"I00","I99",3);  /*��ü ������*/
%DISEASE(heat,"T67","T67",3); /*�¿���ȯ*/
%DISEASE(vol,"E86","E86",3); /*������ Ż�� */
%DISEASE(res,"J00","J99",3);      /*��ü ȣ��� */
%DISEASE(kawa,"M303","M303",4); /*���ͻ�Ű */
%DISEASE(atopic,"L20","L20",3); /*������ �Ǻο� */
/*---------------------------------------------------------------------------------------------------------------------------------------*/
/*****************************************************************************************************/
/*****************************************************************************************************/
/*��¥ �ڷ�, ���� �Ǽ� �ڷ�  missing  ���� ä��� ����*/
data a.ddate; 
format date yymmdd10.;
DO i= 1 to 1826 by 1;
DATE= MDY(01,01,2015)+i-1; 
/* 2015�� 1�� 1�Ϻ��� ~2019�� 12�� 31�� ����*/
output; end;
drop i;
run;

data a.ddate; set a.ddate;
date2=put(date,yymmddn8.); run; /*��¥-> ��������*/

/*��¥ �ڷ�� �õ� �ڷ� ����*/
PROC SQL; CREATE TABLE A.SIDODATE AS SELECT * FROM A.DDATE CROSS JOIN A.SIDO; QUIT;

DATA A.SIDODATE; SET A.SIDODATE;
SIDOKEY=COMPRESS(DATE2)||("-")||COMPRESS(SIDO);
KEEP SIDOKEY SIDO_KN; RUN;

/*����ǥ ���� �����*/
data b.null_tb;
options obs=max;
input category$ Y_2015 Y_2016 Y_2017 Y_2018 Y_2019;
CARDS;
"" "" "" "" "" ""
;
RUN;

/**********************************************************************************************/
/**********************************************************************************************/
/*5���̸� �Ҿ� */

/*
��ũ�� ����
DISEASE : ������ȯ �ڷ�
OUT     : OUTPUT�ڷῡ �ش��ϴ�  DB��
EPI      : ������Ⱓ(���Ǽҵ�) ������ , 7�̸� 7���̳� �ݺ������ �Ѱ�(�������� ���᳻������ �Ǵ�)
*/

/*STEP0*/
/*�ش� ��ȯ �ڰ� ����*/
%MACRO DAILY_COUNT(DISEASE,OUT,EPI);
OPTIONS FIRSTOBS=1 OBS=MAX;
PROC SQL; CREATE TABLE B.D0 AS SELECT * FROM B.&DISEASE. AS A LEFT JOIN A.BFC_R AS B ON A.PKEY =B.PKEY; QUIT;

/**********************************************************************************************/
/*STEP1*/
/*������ Ŭ��¡*/
DATA B.D1; SET B.D0;
IF INDI_DSCM_NO="" THEN DELETE; /*ID ��ȿ ����*/
IF "2015" <= SUBSTR(MDCARE_STRT_DT,1,4) <="2019" AND "01" <=SUBSTR(MDCARE_STRT_DT,5,2) <="12" AND "01" <= SUBSTR(MDCARE_STRT_DT,7,2) <="31"; /*��ȿ�� �������� ����*/
IF "1899" <= SUBSTR(FST_HSPTZ_DT,1,4) <="2019" AND "01" <=SUBSTR(FST_HSPTZ_DT,5,2) <="12" AND "01" <= SUBSTR(FST_HSPTZ_DT,7,2) <="31"                /*��ȿ�� ���� �������� ����*/
THEN FST_HSPTZ_DT=FST_HSPTZ_DT; ELSE FST_HSPTZ_DT="";   /*���� �Կ��� */
IF VSHSP_DD_CNT="" THEN DELETE;                    /*��ȿ�� �Գ��� �ϼ� ����*/
IF VSHSP_DD_CNT=0 THEN VSHSP_DD_CNT=1;     /*�Գ��� �ϼ� 0�� ��� �Կ��� �ߴٰ� ���� ����� ��ħ 0=>1 �ڵ�*/
IF AGE <=4 ;                                                    /*��ȿ ���� ���� (5�� �̸�)*/
IF SEX_TYPE IN ("1","2");                                    /*���� ��ȿ ����*/
IF SIDO="" THEN DELETE; RUN;                         /*�ñ��� ��ȿ ����*/

/**********************************************************************************************/
/*STEP2*/
/*���� ��������, ������ ���*/
DATA B.D2; SET B.D1;
FORMAT MDCARE FST MDCARE_DATE FST_DATE DATE1 YYMMDD10.;

MDCARE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
FST      =MDY(SUBSTR(FST_HSPTZ_DT,5,2),SUBSTR(FST_HSPTZ_DT,7,2),SUBSTR(FST_HSPTZ_DT,1,4));
IF FST^="" THEN FST_STATUS=1; ELSE FST_STATUS=0;
/*���ᰳ������ ���*/
IF MDCARE="" THEN MDCARE_DATE=FST; ELSE MDCARE_DATE=MDCARE;
IF FST^=""      THEN FST_DATE=FST;        ELSE FST_DATE=MDCARE;

DATE1=MIN(FST_DATE,MDCARE_DATE);         /*���� ��������*/
DIFF_PLUS=MDCARE_DATE-DATE1;              /*���ʷ� ���� ���� �� - �Կ���*/
CNT_DD=DIFF_PLUS+VSHSP_DD_CNT;
/*�����ϰ� ���ξ��̵�� ���ο� KEY ����(�ߺ���������)*/
DKEY=COMPRESS(MDCARE_DATE) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO); 
RUN;  /*����� ���*/

/**********************************************************************************************/
/*STEP3*/
/*û�� �Ǽ��� ���޾� �߻��ϸ� �ϳ��� �߻����� ���� �Ͽ� ���*/
/*û�� �Ǵ� ��¥�� ������ ���� �� �Գ��� �ϼ���  KEEP*/
/*���� û�� �Ǵ� ��¥�� ����� �Գ��� �ϼ��� ���ٸ� �ֻ� �� IHD�ΰ͸� KEEP*/
/*���� û�� �Ǵ� ��¥�� ����� �Գ��� �ϼ��� ���� �ֻ��� ������ ���� �ϳ� KEEP*/

/*�Գ��� �ϼ� ��� */
/*PROC SORT DATA=B.D2 ; BY INDI_DSCM_NO MDCARE_DATE CNT_DD SICK_SYM1; RUN; */ /*������ ����*/

/*������ ���� : 1) ��¥+���� ID ��� 2) �Գ��� �ϼ� ���� ���� , 3) ��+�λ� ���� ���� ����*/
PROC SORT DATA=B.D2; BY DKEY DESCENDING CNT_DD DESCENDING ICD_RANK; RUN; 

/*�� ������ ������ ���� ��¥+���� ���̵� �������� ù���� �ƴϸ� ����*/
DATA B.D3; SET B.D2; BY DKEY; IF FIRST.DKEY^=1 THEN DELETE;
DROP DKEY ICD_RANK; RUN;

proc sort data=B.D3; by indi_dscm_no MDCARE_DATE ;run; /*������ ����  ID, ���� ������ ��*/

/**********************************************************************************************/
/*STEP4*/
/*EPISODE ��� */
DATA B.D4; 
RETAIN START_DATE D R K DATE1_DISCHARGE IKEEP; SET B.D3;
FORMAT R START_DATE DATE1_DISCHARGE YYMMDD10.;

BY INDI_DSCM_NO;

/*���� 1 -> Do ~END ��*/
IF FIRST.INDI_DSCM_NO=1 AND LAST.INDI_DSCM_NO=1 THEN DO;
IKEEP=1; R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1;  END;
ELSE DO;

/*���� 2 -> Do ~END ��*/
IF FIRST.INDI_DSCM_NO=1 AND LAST.INDI_DSCM_NO^=1 THEN DO;
IKEEP=1;  R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1;  END; 
ELSE DO;

/*���� 3 -> Do ~END ��*/
/*���ӵ� �Գ��� �ϼ��� ������ ����� ��¥-���� �Կ� ��¥�� ���*/
K=DATE1-R;  
IF K<=&EPI. THEN DO; IKEEP=0; 
IF DATE1+CNT_DD-1 <R THEN D=D; ELSE DO;
R=DATE1+CNT_DD-1; D=R-START_DATE+1;
END; END; ELSE DO;
IKEEP=1;  R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1;
END; END; END;
DATE1_DISCHARGE=DATE1+CNT_DD-1;

IF IKEEP=1 THEN K="." ; 
RUN;

/**********************************************************************************************/
/*STEP5*/
DATA B.D5; SET B.D4;

/*���Ǽҵ� ������ ������ ���̵� (�������� �ش�)*/
IF IKEEP=1; 

/*���� ����*/
IF SEX_TYPE=1 THEN M=1; ELSE M=0;
IF SEX_TYPE=2 THEN F=1; ELSE F=0;

/*���� ����*/
IF AGE=0 THEN AG0=1; ELSE AG0=0; /*0��*/
IF AGE>0 & AGE<=4 THEN AG1=1; ELSE AG1=0; /*1-4��*/

IF AGE=1 THEN AGE1=1; ELSE AGE1=0;
IF AGE=2 THEN AGE2=1; ELSE AGE2=0;
IF AGE=3 THEN AGE3=1; ELSE AGE3=0;
IF AGE=4 THEN AGE4=1; ELSE AGE4=0;

/*�� & ���� ����*/
IF SEX_TYPE=1 & AGE=0 THEN M_AG0=1; ELSE M_AG0=0;
IF SEX_TYPE=1 & AGE^=0 THEN M_AG1=1; ELSE M_AG1=0;

IF SEX_TYPE=2 & AGE=0 THEN F_AG0=1; ELSE F_AG0=0;
IF SEX_TYPE=2 & AGE^=0 THEN F_AG1=1; ELSE F_AG1=0;

KEEP START_DATE CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 MDACRE_STRT_DT MDCARE_DATE SEX_TYPE AGE SIDO SGG CNT_DD 
M F AG0 AG1 M_AG0 M_AG1 F_AG0 F_AG1 AGE1-AGE4;
RUN;

/**********************************************************************************************/
/*STEP6 DAILY COUNT*/
/*sort by id and descending MDCARE_date*/
PROC SORT DATA=B.D5; BY INDI_DSCM_NO DESCENDING MDCARE_DATE DESCENDING CNT_DD; RUN;

PROC SQL; CREATE TABLE B.COUNT AS SELECT START_DATE AS DATE, SIDO, COUNT(START_DATE) AS TOT, SUM(M) AS M, SUM(F)  AS F,
SUM(AG0) AS AG0, SUM(AG1) AS AG1 ,SUM(M_AG0) AS M_AG0, SUM(M_AG1) AS M_AG1, SUM(F_AG0) AS F_AG0, SUM(F_AG1) AS F_AG1
FROM B.D5  GROUP BY START_DATE, SIDO ;QUIT;

PROC SORT DATA= B.COUNT ; BY DATE SIDO; RUN;

DATA B.COUNT;
RETAIN SIDOKEY DATE SIDO; SET B.COUNT;
DATE2=PUT(DATE,YYMMDDN8.);
SIDOKEY=COMPRESS(DATE2)||("-")||COMPRESS(SUBSTR(SIDO,1,2));
DROP DATE2; RUN;

/*�����Ⱓ���� ��¥  ����*/
proc sql; create table B.count2 as select * from a.sidodate as a left join b.count as b on a.sidokey=b.sidokey; quit;

data A.&OUT._dailycount; set b.count2;
format date yymmdd10.;
DATE= MDY(substr(sidokey,5,2),substr(sidokey,7,2),substr(sidokey,1,4));
SIDO= SUBSTR(SIDOKEY,10,2);

RENAME TOT=&OUT._TOT M=&OUT._M F=&OUT._F AG0=&OUT._AG0 AG1=&OUT._AG1
             M_AG0=&OUT._M_AG0 M_AG1=&OUT._M_AG1 F_AG0=&OUT._F_AG0 F_AG1=&OUT._F_AG1;

/*MISSIG VALUE  ä���*/
IF TOT="." THEN TOT=0;
IF M="." THEN M=0;
IF F="." THEN F=0;
IF AG0="." THEN AG0=0;
IF AG1="." THEN AG1=0;
IF M_AG0="." THEN M_AG0=0;
IF M_AG1="." THEN M_AG1=0;
IF F_AG0="." THEN F_AG0=0;
IF F_AG1="." THEN F_AG1=0;
RUN;

/*�󵵼� ��� ����*/
DATA B.Z; SET B.D5;
DATE2=PUT(START_DATE,YYMMDDN8.);
YEAR=SUBSTR(DATE2,1,4);
MONTH=SUBSTR(DATE2,5,2);
INDEX=1;
IF YEAR>=2015 & YEAR<=2019;
RUN;

PROC SQL; CREATE TABLE B.Z1 AS SELECT YEAR, SUM(INDEX) AS TOT, SUM(M) AS MALE, SUM(F) AS FEMALE, SUM(AG0) AS AGE0, 
SUM(AGE1) AS AGE1, SUM(AGE2) AS AGE2 , SUM(AGE3) AS AGE3 , SUM(AGE4) AS AGE4 FROM B.Z GROUP BY YEAR; QUIT;

OPTIONS FIRSTOBS=1 OBS=MAX;
PROC TRANSPOSE DATA=B.Z1 PREFIX=Y_
OUT=B.Z_T;
ID YEAR; RUN;

/*LABEL ����*/
DATA B.Z_T; SET B.Z_T; RENAME _NAME_=CATEGORY; RUN;

/*���̺� ����*/
DATA B.Z_T1; OPTIONS FIRSTOBS=1 OBS=1; SET B.Z_T; RUN; /*��ü*/
DATA B.Z_T2; OPTIONS FIRSTOBS=2 OBS=3; SET B.Z_T; RUN; /*����*/
DATA B.Z_T3; OPTIONS FIRSTOBS=4 OBS=8; SET B.Z_T; RUN; /*���� ��*/

OPTIONS FIRSTOBS=1 OBS=MAX;

/*���ú�*/
PROC SQL; CREATE TABLE B.Z2 AS SELECT YEAR, SIDO,  SUM(INDEX) AS TOT FROM B.Z GROUP BY YEAR, SIDO; QUIT;

PROC SORT DATA=B.Z2; BY SIDO;  RUN;

PROC TRANSPOSE DATA=B.Z2 PREFIX=Y_ 
OUT=B.Z_T4; 
BY SIDO; 
ID YEAR; 
RUN;

DATA B.Z_T4; SET B.Z_T4; 
RENAME SIDO=CATEGORY; 
DROP _NAME_;
RUN;

/*����*/
PROC SQL; CREATE TABLE B.Z3 AS SELECT YEAR, MONTH,  SUM(INDEX) AS TOT FROM B.Z GROUP BY YEAR, MONTH; QUIT;

PROC SORT DATA=B.Z3; BY MONTH;
PROC TRANSPOSE DATA=B.Z3 PREFIX=Y_ 
OUT=B.Z_T5; 
BY MONTH; ID YEAR; 
RUN;

DATA B.Z_T5; SET B.Z_T5; 
RENAME MONTH=CATEGORY; 
DROP _NAME_;
RUN;

DATA A.&OUT._TABLE; SET B.Z_T1 B.NULL_TB B.Z_T2  B.NULL_TB B.Z_T3  B.NULL_TB B.Z_T4  B.NULL_TB B.Z_T5;
IF CATEGORY="" THEN CATEGORY="."; RUN;

/**********************************************************************************************/
/**********************************************************************************************/
/*�ܰ躰 �� �� */
PROC SQL; CREATE TABLE B.S0 AS SELECT COUNT(*)  AS N FROM B.D0; QUIT;
PROC SQL; CREATE TABLE B.S1 AS SELECT COUNT(*)  AS N FROM B.D1; QUIT;
PROC SQL; CREATE TABLE B.S2 AS SELECT COUNT(*)  AS N FROM B.D2; QUIT;
PROC SQL; CREATE TABLE B.S3 AS SELECT COUNT(*)  AS N FROM B.D3; QUIT; 
PROC SQL; CREATE TABLE B.S4 AS SELECT COUNT(*)  AS N FROM B.Z; QUIT;

DATA B.S0; SET B.S0; LABEL="STEP0"; RUN; /*���� ��ȯ �ڷ�*/
DATA B.S1; SET B.S1; LABEL="STEP1"; RUN; /*������ Ŭ��¡*/
DATA B.S2; SET B.S2; LABEL="STEP2"; RUN; /*���ᰳ�� �� ���� ����*/
DATA B.S3; SET B.S3; LABEL="STEP3"; RUN; /*������ �ߺ� ȯ�� ����*/
DATA B.S4; SET B.S4; LABEL="STEP4"; RUN; /*���Ǽҵ� ���� ��*/

/*�ܰ躰 ���� ��ȯ�� N��*/
PROC SQL; CREATE TABLE B.U0 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D0; QUIT;
PROC SQL; CREATE TABLE B.U1 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D1; QUIT;
PROC SQL; CREATE TABLE B.U2 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D2; QUIT;
PROC SQL; CREATE TABLE B.U3 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D3; QUIT; 
PROC SQL; CREATE TABLE B.U4 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.Z; QUIT;

DATA B.U0; SET B.U0; LABEL="STEP0 ���� ��ȯ�� N��"; RUN; /*���� ��ȯ �ڷ� ���� ID��*/
DATA B.U1; SET B.U1; LABEL="STEP1 ���� ��ȯ�� N��"; RUN; /*������ Ŭ��¡ ���� ID��*/
DATA B.U2; SET B.U2; LABEL="STEP2 ���� ��ȯ�� N��"; RUN; /*���ᰳ�� �� ���� ���� ���� ID��*/
DATA B.U3; SET B.U3; LABEL="STEP3 ���� ��ȯ�� N��"; RUN; /*������ �ߺ� ȯ�� ���� ���� ID��*/
DATA B.U4; SET B.U4; LABEL="STEP4 ���� ��ȯ�� N��"; RUN; /*���Ǽҵ� ���� �� ���� ID��*/

/*�ܰ躰  �ڷ� N��, ȯ�ڼ� MERGE*/
DATA A.&OUT._STEP_LEN; SET B.S0-B.S4 B.U0-B.U4; RUN;

/**********************************************************************************************/
/**********************************************************************************************/
/*CASE-CROSSOBER DATA SET*/
data dat; 
set b.d5;
date=put(start_date,yymmddn8.);
EVENT=1;
if substr(date,1,4)>=2015; 
KEEP START_DATE DATE INDI_DSCM_NO SEX_TYPE SIDO AGE EVENT ;
run;

PROC SORT DATA=DAT; BY START_DATE INDI_DSCM_NO ; RUN;

DATA DAT; 
RETAIN ID; SET DAT;
ID =_N_; RUN;

/*control dataset*/
DATA CONTROL_DAT; SET DAT;
EVENT=0;
DO I=-4 TO 4;
CONTROLDATE=intnx("week",START_DATE,i,"s");
if month(start_date)=month(controldate) and controldate ne start_date then output control_dat;
end;
format START_DATE CONTROLDATE YYMMDD10.;
DROP i;
run;

/*case dataset*/
data case_dat; set dat;
controldate=start_date; 
format controldate yymmdd10.;
run;

/*merge*/
data casecross ; set case_dat control_dat;
drop indi_dscm_no; run;

/*ID���� ����*/
PROC SORT DATA=casecross; by id  controldate; run;
data A.&out._casecross; set casecross; run;


%MEND; 

/*5���̸� �Ҿ�  ��ȯ ��ũ��*/
/*���Ǽҵ� 7�� �ϰ�����*/
%DAILY_COUNT(HEAT_C    ,HEAT_CHILD,7);
%DAILY_COUNT(RES_C    ,RES_CHILD,7);
%DAILY_COUNT(CARDI_C    ,CARDI_CHILD,7);
%DAILY_COUNT(VOL_C     ,VOL_CHILD,7);
%DAILY_COUNT(ATOPIC_C,ATOPIC_CHILD,7);
%DAILY_COUNT(KAWA_C  ,KAWA_CHILD,7);
%DAILY_COUNT(feb_C  ,feb_CHILD,7);
%DAILY_COUNT(all_rh_C,all_rh_CHILD,7);
%DAILY_COUNT(pne_C,pne_CHILD,7);
%DAILY_COUNT(asthma_C  ,asthma_CHILD,7);
%DAILY_COUNT(URI_C  ,URI_CHILD,7);
%DAILY_COUNT(ALRI_C ,ALRI_CHILD,7);
%DAILY_COUNT(acute_bronch1_C  ,acute_bronch1_CHILD,7);
%DAILY_COUNT(acute_bronch2_C  ,acute_bronch2_CHILD,7);
%DAILY_COUNT(RSV_C  ,RSV_CHILD,7);
%DAILY_COUNT(om_C  ,om_CHILD,7);
%DAILY_COUNT(intest_C  ,intest_CHILD,7);
%DAILY_COUNT(rota_C  ,rota_CHILD,7);
%DAILY_COUNT(camp_C  ,camp_CHILD,7);
%DAILY_COUNT(HFMD_C  ,HFMD_CHILD,7);
%DAILY_COUNT(mumps_C  ,mumps_CHILD,7);
%DAILY_COUNT(heatrelated_C  ,heatrelated_CHILD,7);


PROC FREQ DATA=B.D5; TABLES SIDO; RUN;

/**********************************************************************/
/*CASECROSSOBER EXAMPLE*/
PROC LOGISTIC DATA=A.CASECROSS;
STRATA ID;
MODEL EVENT=LAG0_MAXTEMP DOW ; RUN;

/**********************************************************************/
/**********************************************************************/
PROC SORT DATA= A.TARGET_MC OUT=Z1 NODUPKEY ;BY MOTHER_ID; RUN;    
PROC SORT DATA= A.TARGET_MC OUT=Z2 NODUPKEY;BY CHILD_ID; RUN;  

PROC SQL; CREATE TABLE Z3 AS SELECT MOTHER_ID, COUNT(MOTHER_ID) AS CNT FROM A.TARGET_MC GROUP BY MOTHER_ID; QUIT;

PROC FREQ DATA=Z3; TABLES CNT; RUN;

/*B���� �۾��ϱ�  */
DATA B.TARGET_MC; SET A.TARGET_MC; RUN;

/*0���ڰ� ���� 5���̸� ����� ���� 0�� �ڷ� ����  N���� �� ����*/
/*BFC_R  ������ �ڰ� �õ����°�� ������ �ڷ�*/
DATA B.BFC_INFANT; SET A.BFC_R;
RENAME PKEY=CHILDKEY

AGE        =C_AGE
SEX_TYPE=C_SEX
SIDO       =C_SIDO
SGG        =C_SGG
INS          =C_INS
INS_G      =C_INS_G
; /*�����ڰ� �̸� ���� */
IF AGE=0;
DROP BYEAR  ;
RUN ;

/*����-���� ���ڷῡ ���� �ڰ� ���̱�(0��) */
PROC SQL; CREATE TABLE B.TARGET_MC2 AS SELECT * FROM B.TARGET_MC AS A LEFT JOIN B.BFC_INFANT AS B ON A.CHILD_ID=B.INDI_DSCM_NO; QUIT;

/*���� �ڰ� ���� ���� ��� ����.. �̰� �Ƹ��� ���� ID�� �ִµ� ��� ��� ���߰ų� ����ؼ� �ڰ� ���� ���°�� */
DATA B.TARGET_MC2; 
RETAIN MOTHERKEY CHILDKEY STD_YYYY MOTHER_ID CHILD_ID ;
SET B.TARGET_MC2;

MOTHERKEY=COMPRESS(substr(left(STD_YYYY),1,4)||compress("-"))||COMPRESS(MOTHER_ID);
IF CHILDKEY^="";
DROP INDI_DSCM_NO;
RUN;
/*���� ���� ���� , ����. ���̵�*/
PROC SORT DATA= B.TARGET_MC2; BY  STD_YYYY MOTHER_ID; RUN;

PROC FREQ DATA=B.TARGET_MC2; TABLES STD_YYYY; RUN;

/*���� �ڰ� ����*/
DATA B.BFC_MOTHER; SET A.BFC_MOTHER;

RENAME 
PKEY=MOTHERKEY
AGE=M_AGE
SIDO=M_SIDO
INS=M_INS
INS_G=M_INS_G
;
DROP INDI_DSCM_NO BYEAR STD_YYYY SEX_TYPE;
RUN;

/*���� �ڰ� MERGE*/
PROC SQL;CREATE TABLE B.TARGET_MC3 AS SELECT * FROM B.TARGET_MC2 AS A LEFT JOIN B.BFC_MOTHER AS B ON
A.MOTHERKEY=B.MOTHERKEY; QUIT;

DATA B.TARGET_MC3; SET B.TARGET_MC3;
IF C_SIDO^="";
IF M_AGE^="";
IF M_SIDO^="";
RUN;

PROC SORT DATA=B.TARGET_MC3; BY STD_YYYY MOTHER_ID; RUN;

ODS OUTPUT LIST=L1;
PROC FREQ DATA=B.TARGET_MC3; TABLES STD_YYYY*C_SEX/LIST; RUN;
ODS OUTPUT CLOSE;

ODS OUTPUT LIST=L2;
PROC FREQ DATA=B.TARGET_MC3; TABLES STD_YYYY*C_SIDO/LIST; RUN;
ODS OUTPUT CLOSE;

ODS OUTPUT LIST=L3;
PROC FREQ DATA=B.TARGET_MC3; TABLES STD_YYYY*M_AGE/LIST; RUN;
ODS OUTPUT CLOSE;

ODS OUTPUT LIST=L4;
PROC FREQ DATA=B.TARGET_MC3; TABLES STD_YYYY*M_SIDO/LIST; RUN;
ODS OUTPUT CLOSE;

/*����� �ڷ� ���᳻�� ����*/
DATA B.T20_CHILD_R; SET A.T20_CHILD;

RENAME INDI_DSCM_NO=CHILD_ID;
/*������+���� ����Ű*/
CHILDKEY=COMPRESS(SUBSTR(LEFT(MDCARE_STRT_DT),1,4))||("-")||COMPRESS(LEFT(INDI_DSCM_NO));

/*�ֻ�, ��1�󺴸� �����*/
/*�� ó�� ���᳻�� �ڵ� Ȯ���ϱ�����*/
DROP  VSHSP_DD_CNT  MDCARE_DD_CNT ; RUN;

/*������ ����� ���� �����߿�  �ش翬�� ����� �����ϴ���  ���������� ã��*/
/*0���϶� ���᳻�� ���� �Ⱓ�� ���͸� �ɰ�*/
PROC SQL; CREATE TABLE B.T20_CHILD_F AS SELECT * FROM B.T20_CHILD_R WHERE CHILDKEY IN (SELECT CHILDKEY FROM B.TARGET_MC3); QUIT;

/*�����ϴ� ����� �� ���� ���� ���� ������ ã��*/
/*���̵�, ���᳻���� �������� ����*/
DATA B.T20_CHILD_F; SET B.T20_CHILD_F;
YEAR=SUBSTR(CHILDKEY,1,4); RUN;

PROC SORT DATA=B.T20_CHILD_F ; BY CHILD_ID  MDCARE_STRT_DT ; RUN;

/*�ش� ����� �������?*/
/*�����ؼ� ���᳻�� ���� �������� �����*/
PROC SORT DATA=B.T20_CHILD_F OUT=B.T20_C_FIRST NODUPKEY ;BY CHILD_ID; RUN;

/*����� ���᳻�� ���� ������ �� � ��ȯ �ڵ带 ������ �ִ���? ������ ��*/
/*PROC FREQ DATA=B.T20_C_FIRST; TABLES SICK_SYM1; RUN;
PROC FREQ DATA=B.T20_C_FIRST; TABLES SICK_SYM2; RUN;
PROC FREQ DATA=B.T20_C_FIRST; TABLES SICK_SYM3; RUN;
PROC FREQ DATA=B.T20_C_FIRST; TABLES SICK_SYM4; RUN;
PROC FREQ DATA=B.T20_C_FIRST; TABLES SICK_SYM5; RUN;*/

/*���̰� ���� ���᳻���� �����Ƿ�(�ӽ� �Ⱓ�� ���ԵǴµ�), �ٵ� ���� ���᳻��(1�Ǹ� ���Ŵ�)*/
DATA B.T20_CHILD_F2; SET B.T20_CHILD_F;

FORMAT MDCARE FST MDCARE_DATE FST_DATE DATE1 YYMMDD10.;

/*��, ��1-4�󺴿� Z38 �ڵ� �����ϴ� ��� ����*/
IF SUBSTR(SICK_SYM1,1,3)="Z38" OR 
   SUBSTR(SICK_SYM2,1,3)="Z38" OR
   SUBSTR(SICK_SYM3,1,3)="Z38" OR
   SUBSTR(SICK_SYM4,1,3)="Z38" OR
   SUBSTR(SICK_SYM5,1,3)="Z38" ; 

/*��簳����, �����Կ��� ��¥ ���� ����*/
MDCARE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
FST      =MDY(SUBSTR(FST_HSPTZ_DT,5,2),SUBSTR(FST_HSPTZ_DT,7,2),SUBSTR(FST_HSPTZ_DT,1,4));

/*�����Կ����� ������ 1, ������ 0*/
IF FST^="" THEN FST_STATUS=1; ELSE FST_STATUS=0;

/*���ᰳ������ ���*/
IF MDCARE="" THEN MDCARE_DATE=FST; ELSE MDCARE_DATE=MDCARE;
IF FST^=""      THEN FST_DATE=FST;        ELSE FST_DATE=MDCARE;

/*��簳�����̶� �����Կ��� �� ���� ��¥�� -> ����Ϸ� ����*/
DATE1=MIN(FST_DATE,MDCARE_DATE);         /*���� ��������*/

/*��¥ ���� -> ������������ ����*/
DATE2=PUT(DATE1,YYMMDDN8.);

/*���� ���� �������� �������� ��/��/��*/
YEAR    =SUBSTR(DATE2,1,4);
MONTH=SUBSTR(DATE2,5,2);
DAY     =SUBSTR(DATE2,7,2);
run;

PROC SORT DATA=B.T20_CHILD_F2 OUT=B.T20_C_FIRST2 NODUPKEY ;BY CHILD_ID DATE1; RUN;

PROC FREQ DATA=B.T20_C_FIRST2; TABLES YEAR; RUN;

/*2015�� ���� ����� ��*/
DATA B.T20_C_FIRST3; SET B.T20_C_FIRST2;
IF FORM_CD="02";
IF YEAR>=2015; 
RUN;

/*�Կ��� ��� ���̵�¥ ���� �����ؼ� ���ʸ� �����*/
PROC SORT DATA=B.T20_C_FIRST3 OUT=B.T20_C_FIRST4 NODUPKEY  ; BY CHILD_ID DATE1; RUN;

DATA B.BABY_Delivery; SET B.T20_C_FIRST4;

RENAME DATE2=Delivery_date;

if SUBSTR(SICK_SYM1,1,4) IN ("Z383","Z384","Z385","Z386","Z387","Z388") OR
 SUBSTR(SICK_SYM2,1 ,4) IN ("Z383","Z384","Z385","Z386","Z387","Z388") OR
 SUBSTR(SICK_SYM3,1 ,4) IN ("Z383","Z384","Z385","Z386","Z387","Z388") OR
 SUBSTR(SICK_SYM4,1 ,4) IN ("Z383","Z384","Z385","Z386","Z387","Z388") OR
 SUBSTR(SICK_SYM5,1 ,4) IN ("Z383","Z384","Z385","Z386","Z387","Z388") THEN C_SINGLETON=0; ELSE C_SINGLETON=1;

KEEP CHILDKEY  DATE2 C_SINGLETON ;
run;

PROC SQL; CREATE TABLE B.TARGET_MC4 AS SELECT * FROM B.TARGET_MC3 AS A LEFT JOIN B.BABY_DELIVERY AS B ON A.CHILDKEY=B.CHILDKEY; QUIT;

/*��� �ֵ� Z38�ڵ� �ִ� ��쿡�� ����� �ٿ����*/
data b.target_mc4; 
RETAIN Motherkey childkey std_yyyy  mother_id child_id  Delivery_date C_SINGLETON; 
set b.target_mc4;
run;

PROC SORT DATA=B.TARGET_MC4; BY childkey DELIVERY_DATE; RUN;

PROC SQL; CREATE TABLE C AS SELECT CHILDKEY, COUNT(CHILDKEY) AS CNT FROM B.TARGET_MC4 GROUP BY CHILDKEY; QUIT;

DATA C1; SET C; IF CNT=1; RUN;
DATA C2; SET C; IF CNT>=2; RUN;

/*�� ����ư� �� �ش� �ѰǸ� �ش��ϴ°��*/
PROC SQL; CREATE TABLE B.C1 AS SELECT * FROM B.TARGET_MC4 WHERE CHILDKEY  IN (SELECT CHILDKEY FROM C1 ); QUIT;
/*�� ����ư� ������ ������ ���*/
PROC SQL; CREATE TABLE B.C2 AS SELECT * FROM B.TARGET_MC4 WHERE CHILDKEY IN (SELECT CHILDKEY FROM C2 ); QUIT;

/*�� ����ư� �� �ش� �ѰǸ� �ش��ϴ°��*/

DATA B.C2_R; SET B.C2; 

IF FIRST.CHILDKEY THEN IKEEP=1;
DATE=MDY(SUBSTR(DELIVERY_DATE,5,2),SUBSTR(DELIVERY_DATE,7,2),SUBSTR(DELIVERY_DATE,1,4));
IF (CHILDKEY=LAG(CHILDKEY)) &  (DATE-LAG(DATE)<280) THEN IKEEP=0; ELSE IKEEP=1; 
IF IKEEP=1; 
DROP IKEEP DATE;
RUN;

DATA B.TARGET_MC5; SET B.C1 B.C2_R; RUN;

PROC SQL; CREATE TABLE Z AS SELECT CHILDKEY, COUNT(CHILDKEY) AS CNT  FROM B.TARGET_MC5 GROUP BY CHILDKEY;QUIT;

PROC FREQ DATA=B.TARGET_MC5; TABLES STD_YYYY*C_SINGLETON/LIST; RUN;

/*���� �и� �ڵ� ���� */
DATA B.T20_MOM_delivery; set b.t20_mom;

/*���� ���� ������ �Կ��̸鼭 O80-O84 �����ϴ� ��� */
if FORM_CD="02" & (SUBSTR(SICK_SYM1,1,3) IN ("O80","O81","O82","O83","O84") OR
  SUBSTR(SICK_SYM2,1,3) IN ("O80","O81","O82","O83","O84") OR
  SUBSTR(SICK_SYM3,1,3) IN ("O80","O81","O82","O83","O84") OR
  SUBSTR(SICK_SYM4,1,3) IN ("O80","O81","O82","O83","O84") OR
  SUBSTR(SICK_SYM5,1,3) IN ("O80","O81","O82","O83","O84")) ;

  /*������ ���� ���� Ű*/
MOTHERKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4))||("-")||COMPRESS(INDI_DSCM_NO);

/*��簳����, �����Կ��� ��¥ ���� ����*/
MDCARE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
FST      =MDY(SUBSTR(FST_HSPTZ_DT,5,2),SUBSTR(FST_HSPTZ_DT,7,2),SUBSTR(FST_HSPTZ_DT,1,4));

/*�����Կ����� ������ 1, ������ 0*/
IF FST^="" THEN FST_STATUS=1; ELSE FST_STATUS=0;

/*���ᰳ������ ���*/
IF MDCARE="" THEN MDCARE_DATE=FST; ELSE MDCARE_DATE=MDCARE;
IF FST^=""      THEN FST_DATE=FST;        ELSE FST_DATE=MDCARE;

/*��簳�����̶� �����Կ��� �� ���� ��¥�� -> ����Ϸ� ����*/
DATE1=MIN(FST_DATE,MDCARE_DATE);         /*���� ��������*/

/*��¥ ���� -> ������������ ����*/
DATE2=PUT(DATE1,YYMMDDN8.);

/*���� ���� �������� �������� ��/��/��*/
YEAR    =SUBSTR(DATE2,1,4);
MONTH=SUBSTR(DATE2,5,2);
DAY     =SUBSTR(DATE2,7,2);

RUN;

/*����-����� ���� �ڷῡ�� ���� ���̵� �����ϴ� ��츸 ã��  */
PROC SQL; CREATE TABLE B.T20_MOM_Delivery2 AS SELECT * FROM B.T20_MOM_delivery WHERE MOTHERKEY IN (SELECT MOTHERKEY FROM B.TARGET_MC4); QUIT;

/*���� �� �ι� �Կ��� ��� ����*/
PROC SORT DATA=B.T20_MOM_DELIVERY2 out=B.T20_MOM_DELIVERY3 nodupkey ; BY INDI_DSCM_NO DATE1; RUN;

/*���� ��� ���� ���� ��� ������ ���� ��� �켱������ �����*/
PROC SORT DATA=B.T20_MOM_DELIVERY3 out=B.T20_MOM_DELIVERY4 nodupkey ; BY INDI_DSCM_NO YYYYMM; RUN;

/*�� �������Լ� ������ �и� ������ ���� �� ����*/
PROC SQL; CREATE TABLE Z  AS SELECT MOTHERKEY, COUNT(MOTHERKEY) AS CNT FROM B.T20_MOM_Delivery4 GROUP BY MOTHERKEY;QUIT;

PROC FREQ DATA=Z; TABLES CNT; RUN;

data z2; set z;
if cnt >=2; run;

PROC SQL; CREATE TABLE B.M1 AS SELECT * FROM B.T20_MOM_DELIVERY4 WHERE MOTHERKEY  NOT IN (SELECT MOTHERKEY FROM Z2 ); QUIT;

PROC SQL; CREATE TABLE Z4 AS SELECT * FROM B.T20_MOM_DELIVERY4 WHERE MOTHERKEY IN (SELECT MOTHERKEY FROM Z2 ); QUIT;

DATA B.M2 ; SET Z4;

/*�� ���ΰ��, ó�� �Կ��ΰ��� 1�̰�,
 �������� ��쿡 ��¥ ���̰� 280�� �̸��̸� ����*/

IF FIRST.INDI_DSCM_NO THEN IKEEP=1;
IF (INDI_DSCM_NO=LAG(INDI_DSCM_NO)) &  (DATE1-LAG(DATE1)<280) THEN IKEEP=0; ELSE IKEEP=1; 
KEEP CMN_KEY INDI_DSCM_NO SICK_SYM1-SICK_SYM5 MDCARE_STRT_DT MOTHERKEY DATE2 IKEEP;
RUN;

/*�������� ��찡 �ƴ϶�� ���ֵǴ°�츸*/
DATA B.M3; SET B.M2; IF IKEEP=1;RUN;

DATA B.M1_R; SET B.M1; KEEP CMN_KEY INDI_DSCM_NO DATE2 SICK_SYM1-SICK_SYM5; RUN;
DATA B.M2_R; SET B.M3; KEEP CMN_KEY INDI_DSCM_NO DATE2 SICK_SYM1-SICK_SYM5; RUN;

/*�и��ڵ� ���� ���� �������Ͽ��� MERGE*/
DATA B.T20_MOM_DELIVERY5; SET B.M1_R B.M2_R; 
MOTHERKEY=COMPRESS(SUBSTR(DATE2,1,4))||("-")||COMPRESS(INDI_DSCM_NO);

/*�и��ڵ尡 O84 �����ϴ� ��� �ֵ���*/
if (SUBSTR(SICK_SYM1,1,3) IN ("O84") OR
  SUBSTR(SICK_SYM2,1,3) IN ("O84") OR
  SUBSTR(SICK_SYM3,1,3) IN ("O84") OR
  SUBSTR(SICK_SYM4,1,3) IN ("O84") OR
  SUBSTR(SICK_SYM5,1,3) IN ("O84")) THEN M_SINGLETON=0; ELSE M_SINGLETON=1;

KEEP MOTHERKEY DATE2 M_SINGLETON; RUN;

/*����-���̽� �ڷ�� ���� �и��ڵ�� �и��� ������ �ڵ� ����*/
PROC SQL; 
CREATE TABLE B.TARGET_MC6 AS SELECT * FROM B.TARGET_MC5 AS A LEFT JOIN B.T20_MOM_DELIVERY5 AS B ON A.MOTHERKEY=B.MOTHERKEY; QUIT;

/*������ ���̿��Ե� ������� Ư���� �� ���� ���� �����ϱ�  */
DATA B.TARGET_MC7;
RETAIN motherkey childkey std_yyyy mother_id child_id delivery_date singleton date2 m_singleton;
SET B.TARGET_MC6;

IF delivery_date="" & date2="" THEN DELETE;
RUN;

PROC SQL; CREATE TABLE Z AS SELECT MOTHERKEY, COUNT(MOTHERkEY) AS CNT FROM B.TARGET_MC7 GROUP BY MOTHERKEY; QUIT;

PROC FREQ DATA= Z; TABLES CNT; RUN;

DATA Z1; SET Z; IF CNT=1; RUN;
DATA Z2; SET Z; IF CNT>=2; RUN;

/*�� ������ ���������� �и��� ���� �ߺ� ����� �� ������ ���ֱ� */
/*���ؿ� �Ѹ� ī��Ʈ �Ǵ� ���*/
PROC SQL; CREATE TABLE B.MC1 AS SELECT * FROM B.TARGET_MC7 WHERE MOTHERkEY  IN (SELECT MOTHERkEY FROM Z1 ); QUIT;
/*�� ����ư� ������ ������ ���*/
PROC SQL; CREATE TABLE B.MC2 AS SELECT * FROM B.TARGET_MC7 WHERE MOTHERkEY IN (SELECT MOTHERkEY FROM Z2 ); QUIT;

DATA B.MC2_R; SET B.MC2;
D1=MDY(SUBSTR(DELIVERY_DATE,5,2),SUBSTR(DELIVERY_DATE,7,2),SUBSTR(DELIVERY_DATE,1,4));
D2=MDY(SUBSTR(DATE2,5,2),SUBSTR(DATE2,7,2),SUBSTR(DATE2,1,4));

/*���� �ڵ忡�� ã�� �и���- �����ڵ忡�� ã�� �и���  */
IF ABS(D1-D2)=0 THEN DELETE;
DROP D1 D2 ;
RUN;

DATA B.TARGET_MC8; 
retain retain motherkey childkey std_yyyy mother_id child_id delivery_date ddate2 ddate ddate m_singleton c_singleton singleton;
SET B.MC1 B.MC2_R;

FORMAT DDATE YYMMDD10.;
D1=MDY(SUBSTR(DELIVERY_DATE,5,2),SUBSTR(DELIVERY_DATE,7,2),SUBSTR(DELIVERY_DATE,1,4));
D2=MDY(SUBSTR(DATE2,5,2),SUBSTR(DATE2,7,2),SUBSTR(DATE2,1,4));

if delivery_date^="" & date2^="" then ddate= min(D1,D2);
if delivery_date^="" & date2="" then ddate=D1;
if delivery_date="" & date2^="" then ddate=D2; 

if m_singleton=1 & c_singleton=1 then singleton=1;
else if m_singleton=1 & c_singleton="" then singleton=1;
else if m_singleton="" & c_singleton=1 then singleton=1; else singleton=0;

DDATE2=PUT(DDATE,YYMMDDN8.);
drop d1 d2;
run;

proc sql; create table zz as select child_id, count(child_id) as cnt from b.target_mc8 group by child_id; quit;

data zz1; set zz; if cnt=1;run;
data zz2; set zz; if cnt>=2;run;
/*���� �и��ڵ�� ���� ������� ������ ���ؿ� �ΰǾ� ������ ������ �𸣴� ��� (���� �⵵�� 1�� 12�� �̷��� �ٴ� ���)   ���� */

proc sql; create table b.target_mc8_1 as select * from b.target_mc8  where child_id in (select child_id from zz1); quit;                                                                   
proc sql; create table b.target_mc8_2 as select * from b.target_mc8  where child_id in (select child_id from zz2); quit;

proc sort data=b.target_mc8_2; by child_id ddate; run;

data b.target_mc8_3; set b.target_mc8_2; 
if delivery_date="" then delete; run;

proc sort data=b.target_mc8_3 out=b.target_mc8_4 nodupkey ;by child_id; run;

data b.target_mc9; set b.target_mc8_1 b.target_mc8_4; run;

proc sort data=b.target_mc9 out=z1 nodupkey ;by child_id; run;
proc sort data=b.target_mc9 out=z2 nodupkey ;by mother_id; run;

proc freq data= b.target_mc9 ; tables ddate; run;

proc freq data=b.target_mc9; tables std_yyyy*singleton/list; run;

/******************************************************************************************/
/******************************************************************************************/
/*���� ���� �ڷ� , ����-���̿���� ����� �����ϰ� ���¾Ƹ� ���� */
DATA B.TARGET_MC_FINAL; 
retain start_exp_date ddate ddate1 ddate2; SET B.TARGET_MC9;

/*�и� �� ����ڵ�� ã�� ���� ���������-280*/
/*�̳�¥ �Ⱓ��  event �߻��ϰ�, �� �� ó���̸� �ֻ�� ��ȯ*/
start_exp_date=ddate-280;
ddate1=put(start_exp_date,yymmddn8.);
rename ddate=delivery_exp_date;

IF SINGLETON=1; 
format start_exp_date yymmdd10.;
drop delivery_date date2 m_singleton c_singleton;
RUN;

proc sort data=b.target_mc_final; by mother_id start_exp_date; run;

DATA A.TARGET_MC_FINAL; SET B.TARGET_MC_FINAL;RUN;

PROC SORT DATA=A.TARGET_MC_FINAL NODUPKEY OUT=Z1; BY mother_id; RUN;
PROC SORT DATA=A.TARGET_MC_FINAL NODUPKEY OUT=Z2; BY CHILD_id; RUN;

PROC FREQ DATA=A.TARGET_MC_FINAL; TABLES std_yyyy; RUN;
PROC FREQ DATA=A.TARGET_MC_FINAL; TABLES STD_YYYY*C_SIDO/LIST; RUN;
PROC FREQ DATA=A.TARGET_MC_FINAL; TABLES STD_YYYY*C_SEX/LIST; RUN;

/*�Ѿ����� ��� ���*/
PROC SQL; CREATE TABLE Z AS SELECT MOTHER_ID , COUNT(MOTHER_ID) AS CNT FROM A.TARGET_MC_FINAL GROUP BY MOTHER_ID; QUIT;
PROC FREQ DATA=Z; TABLES CNT; RUN;

/*���� ������ �����(����)�� ���᳻���� ��������*/
PROC SQL; CREATE TABLE A.T20_MOTHER_TARGET AS SELECT * FROM A.T20_MOTHER WHERE INDI_DSCM_NO IN (SELECT MOTHER_ID FROM A.TARGET_MC_FINAL); QUIT;

/*�������� ���̵� ��������*/
DATA B.TG_MC_F; SET A.TARGET_MC_FINAL;
KEEP mother_id child_id start_exp_date delivery_exp_date motherkey; run;

proc sort data=b.tg_mc_f nodupkey out= b.tg_mc_f; by mother_id ; run;

/*(1) �ֻ�� ��ȯ�� ��; �ӽ� �� ��� ó�� ��ȯ���� �Կ� �湮������  */
%MACRO DISEASE_PREG(OUT,S_CODE,E_CODE,K);
DATA B.DD ; set A.T20_MOTHER_TARGET;
IF FORM_CD IN("02") AND &S_CODE. <= SUBSTR(SICK_SYM1,1,&K.)<=&E_CODE. THEN K1=2; ELSE K1=0; /*�Կ� �̸鼭 �ֻ� COPD �ڵ�, K1�� �ֻ󺴿� �����ϸ� 2*/
IF FORM_CD IN("02") AND &S_CODE. <= SUBSTR(SICK_SYM2,1,&K.)<=&E_CODE. THEN K2=1; ELSE K2=0; /*�Կ� �̸鼭 �λ� COPD �ڵ�, K2�� �λ󺴿� �����ϸ� 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; 

  /*������ ���� ���� Ű*/
MOTHERKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4))||("-")||COMPRESS(INDI_DSCM_NO);

/*��簳����, �����Կ��� ��¥ ���� ����*/
MDCARE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
FST      =MDY(SUBSTR(FST_HSPTZ_DT,5,2),SUBSTR(FST_HSPTZ_DT,7,2),SUBSTR(FST_HSPTZ_DT,1,4));

/*�����Կ����� ������ 1, ������ 0*/
IF FST^="" THEN FST_STATUS=1; ELSE FST_STATUS=0;

/*���ᰳ������ ���*/
IF MDCARE="" THEN MDCARE_DATE=FST; ELSE MDCARE_DATE=MDCARE;
IF FST^=""      THEN FST_DATE=FST;        ELSE FST_DATE=MDCARE;

/*��簳�����̶� �����Կ��� �� ���� ��¥�� -> ����Ϸ� ����*/
DATE1=MIN(FST_DATE,MDCARE_DATE);         /*���� ��������*/

/*��¥ ���� -> ������������ ����*/
DATE2=PUT(DATE1,YYMMDDN8.);
KEEP MOTHERKEY sick_sym1 sick_sym2 indi_dscm_no DATE1 DATE2;
RUN; /*ICD_RANK ��+�λ� �߿䵵 ���� ��Ÿ��*/

/*���� ���� ������¥�� �ش��ϴ� �Կ���*/
PROC SORT DATA=B.dd ; BY indi_dscm_no DATE2; RUN;
PROC SORT DATA=B.dd NODUPKEY; BY indi_dscm_no; RUN;

/*����ڶ� ����*/
proc sql; create table B.&OUT._M AS SELECT * FROM B.TG_MC_F AS A LEFT JOIN B.DD AS B ON A.MOTHERKEY =B.MOTHERKEY; QUIT;

DATA B.&OUT._M ; SET B.&OUT._M;
/*��ȯ�� ��¥ �߻�*/
rename date2=date_&out.;
/*�ӽűⰣ�߿� ��ȯ�� �߻��ϸ� 1, �ƴϸ� 0*/
if start_exp_date<= date1 <=delivery_exp_date then &out.=1; else &out.=0;
format date1 yymmdd10.;
keep motherkey &out. date2;
run;
%MEND;

/*�ֻ�� ��ȯ */
%DISEASE_PREG(oedema,"O10","O16",3);                       /*�ӽ�, ��� �� ���ı� ���� �ܹ鴢 �����м� ��� */
%DISEASE_PREG(preelcamrelated,"O13","O15",3);             /*���ڰ��� */
%DISEASE_PREG(preexisting_hyper,"O10","O10",3);          /*�ӽ�, ��� �� ���ı⿡ �պ��� ���� ���¼� ������ */
%DISEASE_PREG(preeclam_chronic_hyper,"O11","O11",3); /*���� �����п� ��ģ ���ڰ� */
%DISEASE_PREG(gestional_oedema,"O12","O12",3);         /*�������� ������������ �ӽż� ���� �� �ܹ鴢 */
%DISEASE_PREG(gestional_hyper,"O13","O13",3);            /*�ӽ� ������ */
%DISEASE_PREG(preecla,"O14","O14",3);                       /*���ڰ��� */
%DISEASE_PREG(eclampsia,"O15","O15",3);                   /*�ڰ� */
%DISEASE_PREG(matenral_hyper,"O16","O16",3);             /*�󼼺Ҹ��� ��� ������ */
%DISEASE_PREG(rupture_membrane,"O42","O42",3);         /*�縷�� �����Ŀ� */
%DISEASE_PREG(placenta,"O44","O44",3);                     /*��ġ�¹� */
%DISEASE_PREG(sep_placenta,"O45","O45",3);               /*�¹�����ڸ� */


/*�ֻ�� ��ȯ ��*/
PROC FREQ DATA=B.OEDEMA_M;TABLES oedema;RUN;
PROC FREQ DATA=B.preelcamrelated_M;TABLES preelcamrelated;RUN;
PROC FREQ DATA=B.preexisting_hyper_M;TABLES preexisting_hyper;RUN;
PROC FREQ DATA=B.preeclam_chronic_hyper_M;TABLES preeclam_chronic_hyper;RUN;
PROC FREQ DATA=B.gestional_oedema_M;TABLES gestional_oedema;RUN;
PROC FREQ DATA=B.gestional_hyper_M;TABLES gestional_hyper;RUN;
PROC FREQ DATA=B.preecla_M;TABLES preecla;RUN;
PROC FREQ DATA=B.eclampsia_M;TABLES eclampsia;RUN;
PROC FREQ DATA=B.matenral_hyper_M;TABLES matenral_hyper;RUN;
PROC FREQ DATA=B.rupture_membrane_M;TABLES rupture_membrane;RUN;
PROC FREQ DATA=B.placenta_M;TABLES placenta;RUN;
PROC FREQ DATA=B.sep_placenta_M;TABLES sep_placenta;RUN;

/*�ֻ�� ��ȯ merge*/
DATA B.PREG_MOM ;
merge B.OEDEMA_M B.PREELCAMRELATED_M b.preexisting_hyper_m b.preeclam_chronic_hyper_m b.gestional_oedema_m b.gestional_hyper_m 
b.preecla_m b.eclampsia_m b.matenral_hyper_m b.rupture_membrane_m b.placenta_m b.sep_placenta_m; 
by motherkey; run;

proc sql; create table b.preg_mom2 as select * from b.preg_mom as a left join b.tg_mc_f as b on a.motherkey=b.motherkey; quit;

data b.preg_mom2; set b.preg_mom2; key=compress(mother_id)||("-")||compress(child_id); run;
data b.tg_mc_f2; set b.target_mc_final; key=compress(mother_id)||("-")||compress(child_id);run;

/*���� ���¾��� ���鼭 ���̵�, ���ں� ���ĵ� �ڷῡ��*/
/*�ڰ��ڷ� ���� �ʿ��� ������ ��������*/
/*INDEX ��¥�� ���缭 �ӽ� �Ⱓ ��¥  ���� ������ �ٿ��ֱ�*/
DATA B.TG_MC_F3; SET B.TG_MC_F2;
BY MOTHER_ID;
RETAIN INDEX;
IF FIRST.MOTHER_ID THEN INDEX=1;
ELSE INDEX=INDEX+1; 

DROP C_SEX C_AGE C_SIDO C_SGG C_INS C_INS_G M_AGE M_SIDO SGG M_INS M_INS_G ;
RUN;

DATA B.TG_MC_F4; SET B.TG_MC_F3;

IF INDEX=1 THEN PREGDATE1=START_EXP_DATE; 
IF INDEX=1 THEN DLVDATE1   =DELIVERY_EXP_DATE; 

LAG1_DDATE1=LAG(PREGDATE1); LAG1_DDATE2=LAG(DLVDATE1);

IF INDEX=2 THEN PREGDATE1=LAG1_DDATE1;
IF INDEX=2 THEN DLVDATE1  =LAG1_DDATE2;
IF INDEX=2 THEN PREGDATE2=START_EXP_DATE; 
IF INDEX=2 THEN DLVDATE2   =DELIVERY_EXP_DATE;

LAG1_DDATE1=LAG2(PREGDATE1); LAG1_DDATE2=LAG2(DLVDATE1);
LAG2_DDATE1=LAG(PREGDATE2);   LAG2_DDATE2=LAG(DLVDATE2);

IF INDEX=3 THEN PREGDATE1=LAG1_DDATE1;
IF INDEX=3 THEN DLVDATE1  =LAG1_DDATE2;
IF INDEX=3 THEN PREGDATE2=LAG2_DDATE1;
IF INDEX=3 THEN DLVDATE2  =LAG2_DDATE2;
IF INDEX=3 THEN PREGDATE3=START_EXP_DATE; 
IF INDEX=3 THEN DLVDATE3   =DELIVERY_EXP_DATE;

LAG1_DDATE1=LAG3(PREGDATE1); LAG1_DDATE2=LAG3(DLVDATE1);
LAG2_DDATE1=LAG2(PREGDATE2); LAG2_DDATE2=LAG2(DLVDATE2);
LAG3_DDATE1=LAG(PREGDATE3);   LAG3_DDATE2=LAG(DLVDATE3);

IF INDEX=4 THEN PREGDATE1=LAG1_DDATE1;
IF INDEX=4 THEN DLVDATE1  =LAG1_DDATE2;
IF INDEX=4 THEN PREGDATE2=LAG2_DDATE1;
IF INDEX=4 THEN DLVDATE2  =LAG2_DDATE2;
IF INDEX=4 THEN PREGDATE3=LAG3_DDATE1;
IF INDEX=4 THEN DLVDATE3  =LAG3_DDATE2;
IF INDEX=4 THEN PREGDATE4=START_EXP_DATE; 
IF INDEX=4 THEN DLVDATE4   =DELIVERY_EXP_DATE;

LAG1_DDATE1=LAG4(PREGDATE1); LAG1_DDATE2=LAG4(DLVDATE1);
LAG2_DDATE1=LAG3(PREGDATE2); LAG2_DDATE2=LAG3(DLVDATE2);
LAG3_DDATE1=LAG2(PREGDATE3); LAG3_DDATE2=LAG2(DLVDATE3);
LAG4_DDATE1=LAG(PREGDATE4);  LAG4_DDATE2=LAG(DLVDATE4);

IF INDEX=5 THEN PREGDATE1=LAG1_DDATE1;
IF INDEX=5 THEN DLVDATE1  =LAG1_DDATE2;
IF INDEX=5 THEN PREGDATE2=LAG2_DDATE1;
IF INDEX=5 THEN DLVDATE2  =LAG2_DDATE2;
IF INDEX=5 THEN PREGDATE3=LAG3_DDATE1;
IF INDEX=5 THEN DLVDATE3  =LAG3_DDATE2;
IF INDEX=5 THEN PREGDATE4=LAG4_DDATE1;
IF INDEX=5 THEN DLVDATE4  =LAG4_DDATE2;
IF INDEX=5 THEN PREGDATE5=START_EXP_DATE; 
IF INDEX=5 THEN DLVDATE5   =DELIVERY_EXP_DATE;

FORMAT PREGDATE1 YYMMDD10. PREGDATE2 YYMMDD10.  PREGDATE3 YYMMDD10. PREGDATE4 YYMMDD10.  PREGDATE5 YYMMDD10. 
DLVDATE1 YYMMDD10. DLVDATE2 YYMMDD10. DLVDATE3 YYMMDD10. DLVDATE4 YYMMDD10. DLVDATE5 YYMMDD10. ;
DROP LAG1_DDATE1 LAG2_DDATE1 LAG3_DDATE1 LAG4_DDATE1 LAG5_DDATE1 LAG1_DDATE2 LAG2_DDATE2 LAG3_DDATE2 LAG4_DDATE2 LAG5_DDATE2;
RUN;

/*�������� ��� �ӽűⰣ ���������� ������ ���� �ڷḦ ������*/
/*������ �ӽ��ΰ�쿡 ���ؼ� �ٺ����ϱ� ���ĵ� �ڷ� ������ ���� �����*/
PROC SORT DATA=B.TG_MC_F4; BY MOTHER_ID DESCENDING INDEX; RUN;

PROC SORT DATA=B.TG_MC_F4 NODUPKEY OUT=B.TG_MC_F5; BY MOTHER_ID; RUN;

/*���� �������� ã�� ����ڸ� �� �ڷ�� �����Ŀ� �ӽ� ���� ��쿡 �ش��ϴ� ��츸 ã�� */
DATA B.TG_MC_F5; SET B.TG_MC_F5;
KEEP MOTHER_ID INDEX PREGDATE1-PREGDATE5 DLVDATE1-DLVDATE5; RUN;

proc sql; create table b.preg_mom3 as select * from b.preg_mom2 as a left join b.tg_mc_f2 as b on a.key=b.key; quit;

/*�ֻ�� ��ȯ ��ǥ ��ũ��*/
%macro freq_m(diseease);
data z; set b.preg_mom3;
if &diseease.=1; 
year=substr(date_&diseease.,1,4);
month=substr(date_&diseease.,5,2);
day=substr(date_&diseease.,7,2); 
date=mdy(month,day,year);
if m_age <20 then  ag=1;else if
m_age<35 then ag=2;else ag=3;
if year >=2015;
format date yymmdd10.;
keep motherkey &diseease. date year month day m_age m_sido ag ;
run;

PROC SQL; CREATE TABLE C1 AS SELECT year, COUNT(*) AS CNT  FROM z GROUP BY year; RUN;
PROC SQL; CREATE TABLE C2 AS SELECT year,ag, COUNT(*) AS CNT  FROM z GROUP BY year, ag; RUN;
PROC SQL; CREATE TABLE C3 AS SELECT year,m_sido, COUNT(*) AS CNT  FROM z GROUP BY year, m_sido; RUN;
PROC SQL; CREATE TABLE C4 AS SELECT year,month, COUNT(*) AS CNT  FROM z GROUP BY year, month; RUN;

PROC TRANSPOSE DATA=C1 PREFIX=Y_ 
OUT=B.M_T1;  ID YEAR;  RUN;

PROC SORT DATA=C2; BY AG; RUN;
PROC TRANSPOSE DATA=C2 PREFIX=Y_ 
OUT=B.M_T2;  BY AG; ID YEAR; RUN;

PROC SORT DATA=C3; BY M_SIDO;RUN;
PROC TRANSPOSE DATA=C3 PREFIX=Y_ 
OUT=B.M_T3; 
BY M_SIDO; ID YEAR;  RUN;

PROC SORT DATA=C4; BY MONTH;RUN;
PROC TRANSPOSE DATA=C4 PREFIX=Y_ 
OUT=B.M_T4; 
BY MONTH; ID YEAR;  RUN;

DATA B.NULL_TB; SET B.NULL_TB;
RENAME category=_NAME_;RUN;

DATA B.&diseease._M_freq ; SET B.M_T1 B.NULL_TB B.M_T2 B.NULL_TB B.M_T3 B.NULL_TB B.M_T4; RUN;


/*CASE-CROSSOBER DATA SET*/
data dat;  set z;
date2=put(date,yymmddn8.);
EVENT=1;
if substr(date2,1,4)>=2015; 
run;

PROC SORT DATA=DAT; BY DATE motherkey ; RUN;

DATA DAT; 
RETAIN ID; SET DAT;
ID =_N_; RUN;

/*control dataset*/
DATA CONTROL_DAT; SET DAT;
EVENT=0;
DO I=-4 TO 4;
CONTROLDATE=intnx("week",DATE,i,"s");
if month(date)=month(controldate) and controldate ne date then output control_dat;
end;
format  CONTROLDATE YYMMDD10.;
DROP i;
run;

/*case dataset*/
data case_dat; set dat;
controldate=date; 
format controldate yymmdd10.;
run;

/*merge*/
data casecross ; set case_dat control_dat;
drop indi_dscm_no; run;

/*ID���� ����*/
PROC SORT DATA=casecross; by id  controldate; run;
data A.&diseease._casecross; set casecross;
drop year month day ag motherkey &diseease.;
run;

data z; set z;
if m_age <35 then ag1=1; else ag1=0;
if m_age >=35 then ag2=1; else ag2=0;
run;

proc sql; create table B.&diseease._ts as select date as date, M_sido as sido, count(date) as tot_&diseease., sum(ag1) as ag1_&diseease., sum(ag2) as ag2_&diseease. from
z group by date, m_sido; quit;

data B.&diseease._ts ; set B.&diseease._ts;
date2=put(date,yymmddn8.);
sidokey=compress(date2)||("-")||compress(sido);
drop date date2;
run;

proc sql; create table B.&diseease._ts as select * from a.sidodate as a left join B.&diseease._ts as b on a.sidokey=b.sidokey; quit;

data B.&diseease._ts; set B.&diseease._ts;
sido=substr(sidokey,10,2);
if tot_&diseease.="." then tot_&diseease.=0;
if ag1_&diseease.="." then ag1_&diseease.=0;
if ag2_&diseease.="." then ag2_&diseease.=0;
run;
%mend;

%freq_m(oedema);
%freq_m(preelcamrelated);
%freq_m(preexisting_hyper);
%freq_m(preeclam_chronic_hyper);
%freq_m(gestional_oedema);
%freq_m(gestional_hyper);
%freq_m(preecla);
%freq_m(eclampsia);
%freq_m(matenral_hyper);
%freq_m(rupture_membrane);
%freq_m(placenta);
%freq_m(sep_placenta);

/********************************************************************************************************/
/********************************************************************************************************/

/*(2) ���(���� ����) ���� ��ȯ�� ��, �ӽ� �� ��� ������ �Կ� �湮*/
%MACRO DISEASE_PREG2(OUT,S_CODE,E_CODE,K);
DATA B.&OUT._M ; set A.T20_MOTHER_TARGET;
IF FORM_CD IN("02") AND &S_CODE. <= SUBSTR(SICK_SYM1,1,&K.)<=&E_CODE. THEN K1=2; ELSE K1=0; /*�Կ� �̸鼭 �ֻ� COPD �ڵ�, K1�� �ֻ󺴿� �����ϸ� 2*/
IF FORM_CD IN("02") AND &S_CODE. <= SUBSTR(SICK_SYM2,1,&K.)<=&E_CODE. THEN K2=1; ELSE K2=0; /*�Կ� �̸鼭 �λ� COPD �ڵ�, K2�� �λ󺴿� �����ϸ� 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; 
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4)) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);
YEAR=SUBSTR(MDCARE_STRT_DT,1,4);
RUN; /*ICD_RANK ��+�λ� �߿䵵 ���� ��Ÿ��*/
%MEND;

DATA B.heatrelated_M ; set A.T20_MOTHER_TARGET;
IF FORM_CD IN("02") AND SUBSTR(SICK_SYM1,1,3) in ("T67","E86") THEN K1=2; ELSE K1=0; /*�Կ� �̸鼭 �ֻ� COPD �ڵ�, K1�� �ֻ󺴿� �����ϸ� 2*/
IF FORM_CD IN("02") AND SUBSTR(SICK_SYM2,1,3) in ("T67","E86") THEN K2=1; ELSE K2=0; /*�Կ� �̸鼭 �λ� COPD �ڵ�, K2�� �λ󺴿� �����ϸ� 1*/
ICD_RANK=K1+K2; IF ICD_RANK>0; 
PKEY=COMPRESS(SUBSTR(MDCARE_STRT_DT,1,4)) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO);
YEAR=SUBSTR(MDCARE_STRT_DT,1,4);
RUN; /*ICD_RANK ��+�λ� �߿䵵 ���� ��Ÿ��*/

%DISEASE_PREG2(heat,"T67","T67",3)        /*�¿���ȯ*/
%DISEASE_PREG2(VOL,"E86","E86",3)        /*������Ż��*/
%DISEASE_PREG2(intestinal,"A00","A09",3) /*	��ü�尨��*/
%DISEASE_PREG2(cvd,"I00","I99",3)          /*��ü������*/
%DISEASE_PREG2(angina,"I20","I20",3)     /*������*/
%DISEASE_PREG2(IHD,"I20","I25",3)         /*������ ����ȯ*/
%DISEASE_PREG2(MI,"I21","I25",3)          /*�ɱٰ��*/
%DISEASE_PREG2(RESP,"J00","J99",3)    /*��üȣ���*/
%DISEASE_PREG2(URI,"J00","J06",3)       /*��⵵����*/
%DISEASE_PREG2(Pneumonia,"J09","J18",3) /*���*/
%DISEASE_PREG2(asthma,"J45","J46",3)   /*õ��*/
%DISEASE_PREG2(AKI,"N17","N17",3)    /*�޼��żջ�*/

%DISEASE_PREG2(ROTA,"A080","A080",4)    /*��Ÿ���̷���*/
%DISEASE_PREG2(CAMP,"A045","A045",4)    /*į�ʷι��ͼ� ����*/


proc freq data=b.heat_m; tables year; run;
proc freq data=b.vol_m; tables year; run;
proc freq data=b.intestinal_m; tables year; run;
proc freq data=b.cvd_m; tables year; run;
proc freq data=b.angina_m; tables year; run;
proc freq data=b.IHD_m; tables year; run;
proc freq data=b.MI_m; tables year; run;
proc freq data=b.RESP_m; tables year; run;
proc freq data=b.URI_m; tables year; run;
proc freq data=b.Pneumonia_m; tables year; run;
proc freq data=b.asthma_m; tables year; run;
proc freq data=b.aki_m; tables year; run;

proc freq data=A.t20_moTHER ; tables form_cd/list; run;


%MACRO DAILY_COUNT_MOM(DISEASE,OUT,EPI);
OPTIONS FIRSTOBS=1 OBS=MAX;
PROC SQL; CREATE TABLE B.D0 AS SELECT * FROM B.&DISEASE. AS A LEFT JOIN B.BFC_MOTHER AS B ON A.PKEY =B.MOTHERKEY; QUIT;

/**********************************************************************************************/
/*STEP1*/
/*������ Ŭ��¡*/
DATA B.D1; SET B.D0;
IF INDI_DSCM_NO="" THEN DELETE; /*ID ��ȿ ����*/
IF "2015" <= SUBSTR(MDCARE_STRT_DT,1,4) <="2019" AND "01" <=SUBSTR(MDCARE_STRT_DT,5,2) <="12" AND "01" <= SUBSTR(MDCARE_STRT_DT,7,2) <="31"; /*��ȿ�� �������� ����*/
IF "1899" <= SUBSTR(FST_HSPTZ_DT,1,4) <="2019" AND "01" <=SUBSTR(FST_HSPTZ_DT,5,2) <="12" AND "01" <= SUBSTR(FST_HSPTZ_DT,7,2) <="31"                /*��ȿ�� ���� �������� ����*/
THEN FST_HSPTZ_DT=FST_HSPTZ_DT; ELSE FST_HSPTZ_DT="";   /*���� �Կ��� */
IF VSHSP_DD_CNT="" THEN DELETE;                    /*��ȿ�� �Գ��� �ϼ� ����*/
IF VSHSP_DD_CNT=0 THEN VSHSP_DD_CNT=1;     /*�Գ��� �ϼ� 0�� ��� �Կ��� �ߴٰ� ���� ����� ��ħ 0=>1 �ڵ�*/
IF M_SIDO="" THEN DELETE; RUN;                         /*�ñ��� ��ȿ ����*/

/**********************************************************************************************/
/*STEP2*/
/*���� ��������, ������ ���*/
DATA B.D2; SET B.D1;
FORMAT MDCARE FST MDCARE_DATE FST_DATE DATE1 YYMMDD10.;

MDCARE=MDY(SUBSTR(MDCARE_STRT_DT,5,2),SUBSTR(MDCARE_STRT_DT,7,2),SUBSTR(MDCARE_STRT_DT,1,4));
FST      =MDY(SUBSTR(FST_HSPTZ_DT,5,2),SUBSTR(FST_HSPTZ_DT,7,2),SUBSTR(FST_HSPTZ_DT,1,4));
IF FST^="" THEN FST_STATUS=1; ELSE FST_STATUS=0;
/*���ᰳ������ ���*/
IF MDCARE="" THEN MDCARE_DATE=FST; ELSE MDCARE_DATE=MDCARE;
IF FST^=""      THEN FST_DATE=FST;        ELSE FST_DATE=MDCARE;

DATE1=MIN(FST_DATE,MDCARE_DATE);         /*���� ��������*/
DIFF_PLUS=MDCARE_DATE-DATE1;              /*���ʷ� ���� ���� �� - �Կ���*/
CNT_DD=DIFF_PLUS+VSHSP_DD_CNT;
/*�����ϰ� ���ξ��̵�� ���ο� KEY ����(�ߺ���������)*/
DKEY=COMPRESS(MDCARE_DATE) || COMPRESS("-") || COMPRESS(INDI_DSCM_NO); 
RUN;  /*����� ���*/

/**********************************************************************************************/
/*STEP3*/
/*û�� �Ǽ��� ���޾� �߻��ϸ� �ϳ��� �߻����� ���� �Ͽ� ���*/
/*û�� �Ǵ� ��¥�� ������ ���� �� �Գ��� �ϼ���  KEEP*/
/*���� û�� �Ǵ� ��¥�� ����� �Գ��� �ϼ��� ���ٸ� �ֻ� �� IHD�ΰ͸� KEEP*/
/*���� û�� �Ǵ� ��¥�� ����� �Գ��� �ϼ��� ���� �ֻ��� ������ ���� �ϳ� KEEP*/

/*�Գ��� �ϼ� ��� */
/*PROC SORT DATA=B.D2 ; BY INDI_DSCM_NO MDCARE_DATE CNT_DD SICK_SYM1; RUN; */ /*������ ����*/

/*������ ���� : 1) ��¥+���� ID ��� 2) �Գ��� �ϼ� ���� ���� , 3) ��+�λ� ���� ���� ����*/
PROC SORT DATA=B.D2; BY DKEY DESCENDING CNT_DD DESCENDING ICD_RANK; RUN; 

/*�� ������ ������ ���� ��¥+���� ���̵� �������� ù���� �ƴϸ� ����*/
DATA B.D3; SET B.D2; BY DKEY; IF FIRST.DKEY^=1 THEN DELETE;
DROP DKEY ICD_RANK; RUN;

proc sort data=B.D3; by indi_dscm_no MDCARE_DATE ;run; /*������ ����  ID, ���� ������ ��*/

/**********************************************************************************************/
/*STEP4*/
/*EPISODE ��� */
DATA B.D4; 
RETAIN START_DATE D R K DATE1_DISCHARGE IKEEP; SET B.D3;
FORMAT R START_DATE DATE1_DISCHARGE YYMMDD10.;

BY INDI_DSCM_NO;

/*���� 1 -> Do ~END ��*/
IF FIRST.INDI_DSCM_NO=1 AND LAST.INDI_DSCM_NO=1 THEN DO;
IKEEP=1; R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1;  END;
ELSE DO;

/*���� 2 -> Do ~END ��*/
IF FIRST.INDI_DSCM_NO=1 AND LAST.INDI_DSCM_NO^=1 THEN DO;
IKEEP=1;  R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1;  END; 
ELSE DO;

/*���� 3 -> Do ~END ��*/
/*���ӵ� �Գ��� �ϼ��� ������ ����� ��¥-���� �Կ� ��¥�� ���*/
K=DATE1-R;  
IF K<=&EPI. THEN DO; IKEEP=0; 
IF DATE1+CNT_DD-1 <R THEN D=D; ELSE DO;
R=DATE1+CNT_DD-1; D=R-START_DATE+1;
END; END; ELSE DO;
IKEEP=1;  R=DATE1+CNT_DD-1; D=CNT_DD; START_DATE=DATE1;
END; END; END;
DATE1_DISCHARGE=DATE1+CNT_DD-1;

IF IKEEP=1 THEN K="." ; 
RUN;

/**********************************************************************************************/
/*STEP5*/
DATA B.D5; SET B.D4;

/*���Ǽҵ� ������ ������ ���̵� (�������� �ش�)*/
IF IKEEP=1; 
DDATE=PUT(START_DATE,YYMMDDN8.);
MOTHERKEY=compress(substr(ddate,1,4))||("-")||compress(indi_dscm_no);
KEEP  MOTHERKEY START_DATE CMN_KEY INDI_DSCM_NO SICK_SYM1 SICK_SYM2 MDACRE_STRT_DT MDCARE_DATE M_AGE M_SIDO CNT_DD ;
RUN;

/**********************************************************************************************/
/*STEP6 DAILY COUNT*/
/*sort by id and descending MDCARE_date*/
PROC SORT DATA=B.D5; BY INDI_DSCM_NO DESCENDING MDCARE_DATE DESCENDING CNT_DD; RUN;

/*���� �ڷ��  �ӽűⰣ �ش��ϴ� �ڷ�*/
PROC SQL; CREATE TABLE B.D5 AS SELECT * FROM B.D5 AS A LEFT JOIN B.TG_MC_F5 AS B ON A.INDI_DSCM_NO=B.MOTHER_ID; QUIT;

/*�ӽ� �Ⱓ���� �ڷḸ ����*/
/*�� ���� �ִ� 5ȸ*/
DATA B.D6 ; SET B.D5; 
IF PREGDATE1 <= START_DATE <=DLVDATE1 or
PREGDATE2 <= START_DATE <=DLVDATE2 or 
PREGDATE3 <= START_DATE <=DLVDATE3 or 
PREGDATE4 <= START_DATE <=DLVDATE4 or 
PREGDATE5 <= START_DATE <=DLVDATE5  then event=1; ELSE Event=0;
IF EVENT=1;
run;

PROC SQL; CREATE TABLE B.COUNT AS SELECT START_DATE AS DATE, M_SIDO AS SIDO, COUNT(START_DATE) AS TOT 
SUM(AG1) AS AG1, SUM(AG2) AS AG FROM B.D6  GROUP BY START_DATE, M_SIDO ;QUIT;

PROC SORT DATA= B.COUNT ; BY DATE SIDO; RUN;

DATA B.COUNT;
RETAIN SIDOKEY DATE M_SIDO; SET B.COUNT;
DATE2=PUT(DATE,YYMMDDN8.);
SIDOKEY=COMPRESS(DATE2)||("-")||COMPRESS(SUBSTR(SIDO,1,2));
DROP DATE2; RUN;

/*�����Ⱓ���� ��¥  ����*/
proc sql; create table B.count2 as select * from a.sidodate as a left join b.count as b on a.sidokey=b.sidokey; quit;

data A.&OUT._dailycount; set b.count2;
format date yymmdd10.;
DATE= MDY(substr(sidokey,5,2),substr(sidokey,7,2),substr(sidokey,1,4));
M_SIDO= SUBSTR(SIDOKEY,10,2);

RENAME TOT=&OUT._TOT ;

/*MISSIG VALUE  ä���*/
IF TOT="." THEN TOT=0;
RUN;

/*�󵵼� ��� ����*/
DATA B.Z; SET B.D6;
DATE2=PUT(START_DATE,YYMMDDN8.);
YEAR=SUBSTR(DATE2,1,4);
MONTH=SUBSTR(DATE2,5,2);
INDEX=1;
IF YEAR>=2015 & YEAR<=2019;
RUN;

PROC SQL; CREATE TABLE B.Z1 AS SELECT YEAR, SUM(INDEX) AS TOT  FROM B.Z GROUP BY YEAR; QUIT;

OPTIONS FIRSTOBS=1 OBS=MAX;
PROC TRANSPOSE DATA=B.Z1 PREFIX=Y_
OUT=B.Z_T;
ID YEAR; RUN;

/*LABEL ����*/
DATA B.Z_T; SET B.Z_T; RENAME _NAME_=CATEGORY; RUN;

/*���̺� ����*/
DATA B.Z_T1; OPTIONS FIRSTOBS=1 OBS=1; SET B.Z_T; RUN; /*��ü*/


/*���ú�*/
OPTIONS FIRSTOBS=1 OBS=MAX;
PROC SQL; CREATE TABLE B.Z2 AS SELECT YEAR, M_SIDO AS SIDO,  SUM(INDEX) AS TOT FROM B.Z GROUP BY YEAR, M_SIDO; QUIT;

PROC SORT DATA=B.Z2; BY SIDO;  RUN;

PROC TRANSPOSE DATA=B.Z2 PREFIX=Y_ 
OUT=B.Z_T4; 
BY SIDO; 
ID YEAR; 
RUN;

DATA B.Z_T4; SET B.Z_T4; 
RENAME SIDO=CATEGORY; 
DROP _NAME_;
RUN;

/*����*/
PROC SQL; CREATE TABLE B.Z3 AS SELECT YEAR, MONTH,  SUM(INDEX) AS TOT FROM B.Z GROUP BY YEAR, MONTH; QUIT;

PROC SORT DATA=B.Z3; BY MONTH;
PROC TRANSPOSE DATA=B.Z3 PREFIX=Y_ 
OUT=B.Z_T5; 
BY MONTH; ID YEAR; 
RUN;

DATA B.Z_T5; SET B.Z_T5; 
RENAME MONTH=CATEGORY; 
DROP _NAME_;
RUN;

DATA A.&OUT._TABLE; SET B.Z_T1 B.NULL_TB B.NULL_TB B.Z_T4  B.NULL_TB B.Z_T5;
IF CATEGORY="" THEN CATEGORY="."; RUN;

/**********************************************************************************************/
/**********************************************************************************************/
/*�ܰ躰 �� �� */
PROC SQL; CREATE TABLE B.S0 AS SELECT COUNT(*)  AS N FROM B.D0; QUIT;
PROC SQL; CREATE TABLE B.S1 AS SELECT COUNT(*)  AS N FROM B.D1; QUIT;
PROC SQL; CREATE TABLE B.S2 AS SELECT COUNT(*)  AS N FROM B.D2; QUIT;
PROC SQL; CREATE TABLE B.S3 AS SELECT COUNT(*)  AS N FROM B.D3; QUIT; 
PROC SQL; CREATE TABLE B.S4 AS SELECT COUNT(*)  AS N FROM B.D5; QUIT; 
PROC SQL; CREATE TABLE B.S5 AS SELECT COUNT(*)  AS N FROM B.D6; QUIT;

DATA B.S0; SET B.S0; LABEL="STEP0"; RUN; /*���� ��ȯ �ڷ�*/
DATA B.S1; SET B.S1; LABEL="STEP1"; RUN; /*������ Ŭ��¡*/
DATA B.S2; SET B.S2; LABEL="STEP2"; RUN; /*���ᰳ�� �� ���� ����*/
DATA B.S3; SET B.S3; LABEL="STEP3"; RUN; /*������ �ߺ� ȯ�� ����*/
DATA B.S4; SET B.S4; LABEL="STEP4"; RUN; /*���Ǽҵ� ���� ��*/
DATA B.S5; SET B.S5; LABEL="STEP5"; RUN; /*�ӽ� �Ⱓ���� �ڷḸ ����*/

/*�ܰ躰 ���� ��ȯ�� N��*/
PROC SQL; CREATE TABLE B.U0 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D0; QUIT;
PROC SQL; CREATE TABLE B.U1 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D1; QUIT;
PROC SQL; CREATE TABLE B.U2 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D2; QUIT;
PROC SQL; CREATE TABLE B.U3 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D3; QUIT; 
PROC SQL; CREATE TABLE B.U4 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D5; QUIT; 
PROC SQL; CREATE TABLE B.U5 AS SELECT COUNT(distinct(INDI_DSCM_NO))  AS N FROM B.D6; QUIT;

DATA B.U0; SET B.U0; LABEL="STEP0 ���� ��ȯ�� N��"; RUN; /*���� ��ȯ �ڷ� ���� ID��*/
DATA B.U1; SET B.U1; LABEL="STEP1 ���� ��ȯ�� N��"; RUN; /*������ Ŭ��¡ ���� ID��*/
DATA B.U2; SET B.U2; LABEL="STEP2 ���� ��ȯ�� N��"; RUN; /*���ᰳ�� �� ���� ���� ���� ID��*/
DATA B.U3; SET B.U3; LABEL="STEP3 ���� ��ȯ�� N��"; RUN; /*������ �ߺ� ȯ�� ���� ���� ID��*/
DATA B.U4; SET B.U4; LABEL="STEP4 ���� ��ȯ�� N��"; RUN; /*���Ǽҵ� ���� �� ���� ID��*/
DATA B.U5; SET B.U5; LABEL="STEP5 ���� ��ȯ�� N��"; RUN; /*/*�ӽ� �Ⱓ���� �ڷḸ ������ ���� ID��*/

/*�ܰ躰  �ڷ� N��, ȯ�ڼ� MERGE*/
DATA A.&OUT._STEP_LEN; SET B.S0-B.S5 B.U0-B.U5; RUN;

/**********************************************************************************************/
/**********************************************************************************************/
/*CASE-CROSSOBER DATA SET*/
data dat; 
set b.d6;
date=put(start_date,yymmddn8.);
EVENT=1;
if substr(date,1,4)>=2015; 
KEEP START_DATE DATE INDI_DSCM_NO SEX_TYPE SIDO M_AGE EVENT ;
run;

PROC SORT DATA=DAT; BY START_DATE INDI_DSCM_NO ; RUN;

DATA DAT; 
RETAIN ID; SET DAT;
ID =_N_; RUN;

/*control dataset*/
DATA CONTROL_DAT; SET DAT;
EVENT=0;
DO I=-4 TO 4;
CONTROLDATE=intnx("week",START_DATE,i,"s");
if month(start_date)=month(controldate) and controldate ne start_date then output control_dat;
end;
format START_DATE CONTROLDATE YYMMDD10.;
DROP i;
run;

/*case dataset*/
data case_dat; set dat;
controldate=start_date; 
format controldate yymmdd10.;
run;

/*merge*/
data casecross ; set case_dat control_dat;
drop indi_dscm_no; run;

/*ID���� ����*/
PROC SORT DATA=casecross; by id  controldate; run;
data A.&out._casecross; set casecross; run;
%MEND; 

%DAILY_COUNT_MOM(HEATRELATED_M,HEATRELATED_MOTHER,7);
%DAILY_COUNT_MOM(HEAT_M,HEAT_MOTHER,7);
%DAILY_COUNT_MOM(VOL_M,VOL_MOTHER,7);

%DAILY_COUNT_MOM(INTESTINAL_M,INTESTINAL_MOTHER,7);
%DAILY_COUNT_MOM(CVD_M,CVD_MOTHER,7);
%DAILY_COUNT_MOM(angina_M,angina_MOTHER,7);
%DAILY_COUNT_MOM(IHD_M,IHD_MOTHER,7);
%DAILY_COUNT_MOM(MI_M,MI_MOTHER,7);
%DAILY_COUNT_MOM(RESP_M,RESP_MOTHER,7);
%DAILY_COUNT_MOM(URI_M,URI_MOTHER,7);
%DAILY_COUNT_MOM(Pneumonia_M,Pneumonia_MOTHER,7);
%DAILY_COUNT_MOM(asthma_M,asthma_MOTHER,7);
%DAILY_COUNT_MOM(AKI_M,AKI_MOTHER,7);
%DAILY_COUNT_MOM(ROTA_M,ROTA_MOTHER,7);
%DAILY_COUNT_MOM(CAMP_M,CAMP_MOTHER,7);
%DAILY_COUNT_MOM(VOL_M,VOL_MOTHER,7);

