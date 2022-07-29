ZhejiangProvinceICU
================
ZZH
2022-05-19

\#medical history EMR

``` r
EMREICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU20120101-20191231/病历.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 668 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (26): patient_SN, 病案号, 住院号, 性别, 民族, 就诊标识（医渡云计算）, 就诊类型, 主诉（24h内入出院记录）, 入...
    ## dbl   (3): 门诊号, 年龄, 住院天数
    ## lgl   (2): 主诉（24小时内入院死亡记录）, 入院情况（24小时内入院死亡记录）
    ## dttm  (3): 出生日期, 入院(就诊)时间, 出院时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
EMREICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU20200101-20220518/病历.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 884 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (24): patient_SN, 性别, 民族, 就诊标识（医渡云计算）, 就诊类型, 主诉（24h内入出院记录）, 入院情况（24h内入出...
    ## dbl   (4): 病案号, 住院号, 年龄, 住院天数
    ## lgl   (3): 门诊号, 主诉（24小时内入院死亡记录）, 入院情况（24小时内入院死亡记录）...
    ## dttm  (3): 出生日期, 入院(就诊)时间, 出院时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
EMRICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/ICU20120101-20191231/病历.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 4033 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (26): patient_SN, 病案号, 住院号, 性别, 民族, 就诊标识（医渡云计算）, 就诊类型, 主诉（24h内入出院记录）, 入...
    ## dbl   (2): 年龄, 住院天数
    ## lgl   (3): 门诊号, 主诉（24小时内入院死亡记录）, 入院情况（24小时内入院死亡记录）...
    ## dttm  (3): 出生日期, 入院(就诊)时间, 出院时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
EMRICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/ICU20200101-20220519/病历.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 2546 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (26): patient_SN, 性别, 民族, 就诊标识（医渡云计算）, 就诊类型, 主诉（24h内入出院记录）, 入院情况（24h内入出...
    ## dbl   (4): 病案号, 住院号, 年龄, 住院天数
    ## lgl   (1): 门诊号
    ## dttm  (3): 出生日期, 入院(就诊)时间, 出院时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
EMR_orig <- rbind(
  EMREICU1,EMREICU2,EMRICU1,EMRICU2
) %>% 
   select(
     patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    Sex = `性别`,
    DOB = `出生日期`,
    Age = `年龄`, HospitalAdmissionTime = `入院(就诊)时间`,
    ChiefComplain_24hr = `主诉（24h内入出院记录）`,
    AdmissionStatus_24hr = `入院情况（24h内入出院记录）`,
    ChiefComplain_24hr_dead = `主诉（24小时内入院死亡记录）`,
    AdmissionStatus_24hr_dead = `入院情况（24小时内入院死亡记录）`,
    ChiefComplain = `主诉`, Med_history = `现病史`,
    PastHistory = "既往疾病",
    StatusOnDischarge = "转归情况",
    DiagnosisOnDeath = "死亡诊断",
    StatusOnDischarge_Desc = "出院情况",
    DischargeTime = "出院时间", DaysHospitalStay = "住院天数"
  ) %>% 
   mutate(
     Sex = recode(Sex, 女 ="Female", 男 = "Male"),
     StatusOnDischarge = recode(
       StatusOnDischarge,
       不选 = "Unknown",其他 = "Others", 治愈 = "Cured",
       好转 = "Recovered",
       未愈 = "Not cured",死亡 = "Dead",
       `未愈，再次办理出入院。` = "Readmission",
       `自动出院` = "Discharge against medical order",
       `未愈。` ="Not cured",`病情改善` = "Improved",
       "签字后自动出院。" = "Discharge against medical order",
       其它 = "Others", "治愈\n疗效评价：治愈" = "Cured",
       "好转。" = "Recovered","自动离院。"="Discharge against medical order",
       "未愈（本科）。" = "Not cured",
       "其他自动出院，转当地ICU" = "Discharge against medical order",
       "其他自动出院" = "Discharge against medical order",
       "其他转院康复治疗" = "Discharge to rehabilitation center"
     )
     ) 
#translate chief complain
write(
  EMR_orig$ChiefComplain,
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/ChiefComplain.csv"
  )
ChiefComplain_Eng2Chin <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/ChiefComplain1653009249003.xlsx",
  col_names = c("ChiefComplain_Eng","ChiefComplain_Chin")
) %>% mutate(
  ChiefComplain_Chin = EMR_orig$ChiefComplain
) %>% 
  distinct(ChiefComplain_Chin,.keep_all = T)

#第二次数据有增加
EMR1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/病历.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 1543 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (26): patient_SN, 病案号, 住院号, 性别, 民族, 就诊标识（医渡云计算）, 就诊类型, 主诉（24h内入出院记录）, 入...
    ## dbl   (2): 年龄, 住院天数
    ## lgl   (3): 门诊号, 主诉（24小时内入院死亡记录）, 入院情况（24小时内入院死亡记录）...
    ## dttm  (3): 出生日期, 入院(就诊)时间, 出院时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
EMR2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/病历.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 2490 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (24): patient_SN, 性别, 民族, 就诊标识（医渡云计算）, 就诊类型, 主诉（24h内入出院记录）, 入院情况（24h内入出...
    ## dbl   (4): 病案号, 住院号, 年龄, 住院天数
    ## lgl   (3): 门诊号, 主诉（24小时内入院死亡记录）, 入院情况（24小时内入院死亡记录）...
    ## dttm  (3): 出生日期, 入院(就诊)时间, 出院时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
EMR3 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/病历.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Warning: One or more parsing issues, see `problems()` for details

    ## Rows: 2546 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (24): patient_SN, 性别, 民族, 就诊标识（医渡云计算）, 就诊类型, 主诉（24h内入出院记录）, 入院情况（24h内入出...
    ## dbl   (4): 病案号, 住院号, 年龄, 住院天数
    ## lgl   (3): 门诊号, 主诉（24小时内入院死亡记录）, 入院情况（24小时内入院死亡记录）...
    ## dttm  (3): 出生日期, 入院(就诊)时间, 出院时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
EMR4 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/病历.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 1627 Columns: 34
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (26): patient_SN, 病案号, 住院号, 性别, 民族, 就诊标识（医渡云计算）, 就诊类型, 主诉（24h内入出院记录）, 入...
    ## dbl   (3): 门诊号, 年龄, 住院天数
    ## lgl   (2): 主诉（24小时内入院死亡记录）, 入院情况（24小时内入院死亡记录）
    ## dttm  (3): 出生日期, 入院(就诊)时间, 出院时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
EMR_add <- rbind(
  EMR1,EMR2,EMR3,EMR4
) %>% 
   select(
     patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    Sex = `性别`,
    DOB = `出生日期`,
    Age = `年龄`, HospitalAdmissionTime = `入院(就诊)时间`,
    ChiefComplain_24hr = `主诉（24h内入出院记录）`,
    AdmissionStatus_24hr = `入院情况（24h内入出院记录）`,
    ChiefComplain_24hr_dead = `主诉（24小时内入院死亡记录）`,
    AdmissionStatus_24hr_dead = `入院情况（24小时内入院死亡记录）`,
    ChiefComplain = `主诉`, Med_history = `现病史`,
    PastHistory = "既往疾病",
    StatusOnDischarge = "转归情况",
    DiagnosisOnDeath = "死亡诊断",
    StatusOnDischarge_Desc = "出院情况",
    DischargeTime = "出院时间", DaysHospitalStay = "住院天数"
  ) %>% 
   mutate(
     Sex = recode(Sex, 女 ="Female", 男 = "Male",`女|男` = "Male"),
     StatusOnDischarge = recode(
       StatusOnDischarge,
       不选 = "Unknown",其他 = "Others", 治愈 = "Cured",
       好转 = "Recovered",
       未愈 = "Not cured",死亡 = "Dead",
       `未愈，再次办理出入院。` = "Readmission",
       `自动出院` = "Discharge against medical order",
       `未愈。` ="Not cured",`病情改善` = "Improved",
       "签字后自动出院。" = "Discharge against medical order",
       其它 = "Others", "治愈\n疗效评价：治愈" = "Cured",
       "好转。" = "Recovered","自动离院。"="Discharge against medical order",
       "未愈（本科）。" = "Not cured",
       "其他自动出院，转当地ICU" = "Discharge against medical order",
       "其他自动出院" = "Discharge against medical order",
       "其他转院康复治疗" = "Discharge to rehabilitation center"
     ),
     StatusOnDischarge = if_else(is.na(DiagnosisOnDeath),StatusOnDischarge,"Dead")
     ) 
EMR <- EMR_add %>% 
  left_join(
    ChiefComplain_Eng2Chin,
    by = c("ChiefComplain" = "ChiefComplain_Chin")
  ) %>% 
  distinct(Hospital_ID,.keep_all = T) %>% 
  mutate(Age_cut = cut(Age, breaks = c(0,18,45,65,75,90,150)),
         DischargeTime = as.duration(HospitalAdmissionTime %--% DischargeTime)/ddays(1),
         #remove year from text for de-identification
         Med_history = str_replace_all(Med_history,"20[:digit:]{2}","****")
  )
  
ZeroTime <- EMR %>% 
  select(Hospital_ID,HospitalAdmissionTime)
EMR <- EMR %>% 
  select(-c(HospitalAdmissionTime,DOB,Age))
write.csv(
  EMR,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/EMR.csv"
)
#EMR用于给护理记录单赋值
NursingChartMatch <- rbind(
  EMR1,EMR2,EMR3,EMR4
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    MedID =`病案号`,
    HosID = `住院号`
  )
```

\#医嘱信息

``` r
MedOrder_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/医嘱.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 570109 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 医嘱内容, 是否长期医嘱...
    ## dttm (4): 医嘱开始时间, 医嘱结束时间, 医嘱开立时间, 医嘱停止时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
MedOrder_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/医嘱.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 866690 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 医嘱内容, 是否长期医嘱...
    ## dbl  (1): 病案号
    ## dttm (4): 医嘱开始时间, 医嘱结束时间, 医嘱开立时间, 医嘱停止时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
MedOrder_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/医嘱.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 919353 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 医嘱内容, 是否长期医嘱...
    ## dbl  (1): 病案号
    ## dttm (4): 医嘱开始时间, 医嘱结束时间, 医嘱开立时间, 医嘱停止时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
MedOrder_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/医嘱.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 571140 Columns: 11
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 医嘱内容, 是否长期医嘱...
    ## dttm (4): 医嘱开始时间, 医嘱结束时间, 医嘱开立时间, 医嘱停止时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
MedOrder <- rbind(
  MedOrder_EICU1,MedOrder_EICU2,
  MedOrder_ICU1,MedOrder_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    MedOrder_Type = `是否长期医嘱`,
    MedOrder_DESC = `医嘱内容`,
    MedOrder_Start_DateTime = `医嘱开始时间`,
    MedOrder_Stop_DateTime = `医嘱结束时间`
  ) %>% 
  mutate(
    MedOrder_Type = recode(MedOrder_Type, 
                           临时 ="STAT", 
                           出院带药 = "OnDischarge",
                           长期 = "Regular")
  ) %>% 
  left_join(ZeroTime) %>% 
  mutate(
    MedOrder_Start_DateTime = as.duration(HospitalAdmissionTime %--%MedOrder_Start_DateTime)/ddays(1),
    MedOrder_Stop_DateTime = as.duration(HospitalAdmissionTime%--%MedOrder_Stop_DateTime)/ddays(1)
  ) %>% 
  select(-HospitalAdmissionTime) %>% 
  filter(!is.na(MedOrder_DESC))
```

    ## Joining, by = "Hospital_ID"

``` r
table(MedOrder$patient_SN%in%EMR$patient_SN)
```

    ## 
    ##    TRUE 
    ## 1741314

``` r
table(MedOrder$Hospital_ID%in%EMR$Hospital_ID)
```

    ## 
    ##    TRUE 
    ## 1741314

``` r
write(
  unique(MedOrder$MedOrder_DESC),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/MedOrder.csv"
  )
write.csv(
  MedOrder,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/MedOrder.csv"
)
```

\#检验信息

``` r
Lab_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/检验.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 2260078 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (13): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 检验分类, 检验子项名称, 检验结果(定性), 检...
    ## dbl   (1): 检验结果(定量)
    ## dttm  (3): 检验报告时间, 检验标本采样时间, 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Lab_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/检验.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 3364879 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 检验分类, 检验子项名称, 检验结果(定性), 检验单位, ...
    ## dbl   (2): 病案号, 检验结果(定量)
    ## dttm  (3): 检验报告时间, 检验标本采样时间, 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Lab_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/检验.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 3313471 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 检验分类, 检验子项名称, 检验结果(定性), 检验单位, ...
    ## dbl   (2): 病案号, 检验结果(定量)
    ## dttm  (3): 检验报告时间, 检验标本采样时间, 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Lab_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/检验.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 2144054 Columns: 17
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (13): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 检验分类, 检验子项名称, 检验结果(定性), 检...
    ## dbl   (1): 检验结果(定量)
    ## dttm  (3): 检验报告时间, 检验标本采样时间, 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Lab <- rbind(
  Lab_EICU1,Lab_EICU2,
  Lab_ICU1,Lab_ICU2
) %>% 
   select(
     patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    "Lab_category" = `检验分类`,
    "Lab_time"="检验报告时间",
    "Lab_itemName"="检验子项名称","Lab_results"="检验结果(定量)",
    "Unit_measure" = "检验单位", 
    "Sample" = "检验标本",
    "LabSampleCollect_time" ="检验标本采样时间") %>% 
  mutate(
    Lab_category = recode(Lab_category,
      "临床化学检验类" = "Clinical chemistry lab.",
      "临床体液、血液学检查" = "Clinical body fluid/hematology",
      "临床微生物与寄生虫检验" = "Clinical microbiology and parasite",
      "临床免疫学检验" = "Clinical immunology lab"
    )
  )
write(
  unique(Lab$Lab_itemName),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Lab_itemName.csv"
  )
write(
  unique(Lab$Sample),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Lab_Sample.csv"
  )
Lab_itemName_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Lab_itemName_Eng.xlsx",
  col_names = c("Lab_itemName_Eng")
) %>% mutate(
  Lab_itemName_Chin = unique(Lab$Lab_itemName)
)
Lab_Sample_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Lab_SampleEng.xlsx",
  col_names = c("Lab_Sample","Lab_Sample_Eng")
)
Lab <- Lab %>% 
  left_join(
    Lab_itemName_Chin2Eng,
    by = c("Lab_itemName"="Lab_itemName_Chin")
  ) %>% 
  left_join(
    Lab_Sample_Chin2Eng,
    by = c("Sample"="Lab_Sample")
  ) %>%
  left_join(ZeroTime) %>% 
  mutate(Lab_time=as.duration(HospitalAdmissionTime%--%Lab_time)/ddays(1),
         LabSampleCollect_time =as.duration(HospitalAdmissionTime%--%LabSampleCollect_time)/ddays(1)
         ) %>% 
  select(-c(Lab_itemName,Sample,HospitalAdmissionTime))
```

    ## Joining, by = "Hospital_ID"

``` r
table(Lab$patient_SN %in% EMR$patient_SN )
```

    ## 
    ##     TRUE 
    ## 11082482

``` r
table(Lab$Hospital_ID %in% EMR$Hospital_ID )
```

    ## 
    ##     TRUE 
    ## 11082482

``` r
write.csv(
  Lab,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/Lab.csv"
)
```

\#Diagnosis

``` r
Diagnosis_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/诊断.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 34005 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 是否药敏试验, 诊断名称, ICD10编码, ICD...
    ## dttm (1): 诊断时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Diagnosis_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/诊断.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 61828 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 是否药敏试验, 诊断名称, ICD10编码, ICD10名称...
    ## dbl  (1): 病案号
    ## dttm (1): 诊断时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Diagnosis_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/诊断.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 74447 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 是否药敏试验, 诊断名称, ICD10编码, ICD10名称...
    ## dbl  (1): 病案号
    ## dttm (1): 诊断时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Diagnosis_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/诊断.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 38898 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 是否药敏试验, 诊断名称, ICD10编码, ICD...
    ## dttm (1): 诊断时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Diagnosis <- rbind(
  Diagnosis_EICU1,Diagnosis_EICU2,
  Diagnosis_ICU1,Diagnosis_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    Diagnosis_Desc = '诊断名称',
    ICD10_code = 'ICD10编码',
    ICD10_name = 'ICD10名称',
    Diagnosis_DateTime = `诊断时间`
  ) %>% 
  left_join(ZeroTime) %>% 
  mutate(Diagnosis_DateTime = as.duration(HospitalAdmissionTime %--% Diagnosis_DateTime)/ddays(1) ) %>% 
  select(-HospitalAdmissionTime)
```

    ## Joining, by = "Hospital_ID"

``` r
table(Diagnosis$Hospital_ID%in%EMR$Hospital_ID)
```

    ## 
    ##   TRUE 
    ## 209178

``` r
table(EMR$Hospital_ID%in%Diagnosis$Hospital_ID)
```

    ## 
    ## FALSE  TRUE 
    ##    13  8167

``` r
write.csv(
  Diagnosis,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/Diagnosis.csv"
)
```

\#生命体征

``` r
VitalSign_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/生命体征.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 764995 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 生命体征子类名称, 生命体征查体值, 生命体征子类单...
    ## dttm (1): 查体时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
VitalSign_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/生命体征.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 1309621 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 生命体征子类名称, 生命体征查体值, 生命体征子类单位...
    ## dbl  (1): 病案号
    ## dttm (1): 查体时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
VitalSign_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/生命体征.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 2382083 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 生命体征子类名称, 生命体征查体值, 生命体征子类单位...
    ## dbl  (1): 病案号
    ## dttm (1): 查体时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
VitalSign_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/生命体征.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 1109076 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (8): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 生命体征子类名称, 生命体征查体值, 生命体征子类单...
    ## dttm (1): 查体时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
VitalSign <- rbind(
  VitalSign_EICU1,VitalSign_EICU2,
  VitalSign_ICU1,VitalSign_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    VitalSign_DESC = '生命体征子类名称',
    VitalSign_value = '生命体征查体值',
    VitalSign_unit = '生命体征子类单位',
    VitalSign_time = '查体时间'
  ) %>% 
  mutate(
    VitalSign_DESC = recode(
      VitalSign_DESC,
      心率 = "Heart Rate",
      舒张压 = "Diastolic Blood pressure",
      收缩压 = "Systolic Blood pressure",
      体重 = "Body weight",
      呼吸 = "Respiratory rate", 身高 = "Height",
      体温 = "Temperature",
      血氧饱和度 = "Oxygen saturation (Pulse Oxymetry)",
      脉搏 = "Pulse rate"
    ),
    VitalSign_unit = recode(
      VitalSign_unit,
      `次/分` = "/min",`分/次` = "/min",`%^` = "%",
      `℃` = "°C",
      `01257070%`="%"
    )
  ) %>% 
  left_join(ZeroTime) %>% 
  mutate(VitalSign_time = as.duration(HospitalAdmissionTime %--% VitalSign_time)/ddays(1) ) %>% 
  select(-HospitalAdmissionTime)
```

    ## Joining, by = "Hospital_ID"

``` r
table(VitalSign$patient_SN%in%EMR$patient_SN)
```

    ## 
    ##    TRUE 
    ## 5565775

``` r
write.csv(
  VitalSign,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/VitalSign.csv"
)
```

\#hospital transfer

``` r
HospitalTransfer_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/转科历史.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 999 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 转入科室, 转出科室...
    ## dttm (2): 转入日期时间, 转出日期时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
HospitalTransfer_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/转科历史.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 3618 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 转入科室, 转出科室...
    ## dbl  (1): 病案号
    ## dttm (2): 转入日期时间, 转出日期时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
HospitalTransfer_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/转科历史.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 3757 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (6): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 转入科室, 转出科室...
    ## dbl  (1): 病案号
    ## dttm (2): 转入日期时间, 转出日期时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
HospitalTransfer_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/转科历史.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 1294 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (7): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 转入科室, 转出科室...
    ## dttm (2): 转入日期时间, 转出日期时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
HospitalTransfer <- rbind(
  HospitalTransfer_EICU1,HospitalTransfer_EICU2,
  HospitalTransfer_ICU1,HospitalTransfer_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    TransferIn_dateTime = '转入日期时间',
    TransferOut_dateTime = '转出日期时间',
    TransferTo_Dept = '转入科室',
    TransferFrom_Dept = '转出科室'
  )
write(
  unique(c(HospitalTransfer$TransferTo_Dept,HospitalTransfer$TransferFrom_Dept)),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/HospitalTransfer.csv"
  )
HospitalTransfer_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/HospitalTransfer.xlsx",
  col_names = c("Transfer_Dept","Transfer_Dept_Eng")
)
HospitalTransfer <- HospitalTransfer %>% 
  left_join(
    HospitalTransfer_Chin2Eng,
    by = c("TransferTo_Dept"="Transfer_Dept")
  ) %>% 
  rename("TransferTo_Dept_Eng" = Transfer_Dept_Eng) %>% 
  left_join(
    HospitalTransfer_Chin2Eng,
    by = c("TransferFrom_Dept"="Transfer_Dept")
  ) %>% 
  rename("TransferFrom_Dept_Eng" = Transfer_Dept_Eng) %>% 
  left_join(ZeroTime) %>% 
  mutate(TransferIn_dateTime = as.duration(HospitalAdmissionTime %--% TransferIn_dateTime)/ddays(1),
         TransferOut_dateTime = as.duration(HospitalAdmissionTime %--% TransferOut_dateTime)/ddays(1)
         ) %>% 
  select(-c(TransferTo_Dept,TransferFrom_Dept,HospitalAdmissionTime))
```

    ## Joining, by = "Hospital_ID"

``` r
table(HospitalTransfer$Hospital_ID %in% EMR$Hospital_ID)
```

    ## 
    ## TRUE 
    ## 9668

``` r
write.csv(
  HospitalTransfer,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/HospitalTransfer.csv"
)
```

\#Medication

``` r
Medication_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组11/用药.csv",
  locale=readr::locale(encoding="GB18030")
) %>% 
  mutate(
    `用药（通用名称）` = as.character(`用药（通用名称）`),
    `用药（通用名称）` = if_else(is.na(`用药（通用名称）`), `用药（商品名称）`, `用药（通用名称）`)
  ) %>% 
  select(-`用药（商品名称）`)
```

    ## Rows: 313928 Columns: 21
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (14): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 用药计量单位, 用药（商品名称）, 药品类别, 用...
    ## dbl   (1): 单次剂量
    ## lgl   (2): 用药时段, 用药（通用名称）
    ## dttm  (4): 给药开始时间, 给药停止时间, 医嘱开立时间, 医嘱停止时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Medication_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/用药.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 504563 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 用药（通用名称）, 药品类别, 用药频率, 用药计量单位, 给...
    ## dbl  (2): 病案号, 单次剂量
    ## dttm (2): 给药开始时间, 给药停止时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Medication_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/用药.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 541481 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (9): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 用药（通用名称）, 药品类别, 用药频率, 用药计量单位, 给...
    ## dbl  (2): 病案号, 单次剂量
    ## dttm (2): 给药开始时间, 给药停止时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Medication_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/用药.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 308786 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 用药（通用名称）, 药品类别, 用药频率, 用药计...
    ## dbl   (1): 单次剂量
    ## dttm  (2): 给药开始时间, 给药停止时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
Medication <- rbind(
  Medication_EICU1 %>% select(names(Medication_EICU2)),
  Medication_EICU2,
  Medication_ICU1,Medication_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    Med_category = "药品类别",
    Med_DESC = "用药（通用名称）",
    SingleDose = "单次剂量",
    Med_Freq = "用药频率",
    Med_unit = "用药计量单位",
    Med_route = "给药方式",
    Med_startTime = "给药开始时间",
    Med_stopTime = "给药停止时间"
  ) %>% 
  mutate(
    Med_category = recode(Med_category,
      "西药" = "Western medicine",
      "中成药" = "Chinese traditional medicine"
    )
  ) 
write(
  unique(Medication$Med_route),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Med_route.csv"
  )
MedicationRoute_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Med_route.xlsx",
  col_names = c("Med_route_Chin","Med_route_Eng")
)
write(
  unique(Medication$Med_DESC),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Med_DESC.csv"
  )
Med_DESC_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Med_DESC.xlsx",
  col_names = c("Med_DESC_Chin","Med_DESC_Eng")
)
Medication <- Medication %>% 
  left_join(
    MedicationRoute_Chin2Eng,
    by = c("Med_route" = "Med_route_Chin")
  ) %>% 
  left_join(
    Med_DESC_Chin2Eng,
    by = c("Med_DESC" = "Med_DESC_Chin")
  ) %>% 
  left_join(ZeroTime) %>% 
  mutate(Med_startTime = as.duration(HospitalAdmissionTime %--% Med_startTime)/ddays(1),
         Med_stopTime = as.duration(HospitalAdmissionTime %--% Med_stopTime)/ddays(1)
         ) %>% 
  select(-c(Med_route,HospitalAdmissionTime,Med_DESC))
```

    ## Joining, by = "Hospital_ID"

``` r
write.csv(
  Medication,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/Medication.csv"
)
```

\#检查

``` r
ExamReport_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/检查.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 16299 Columns: 28
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 检查分类, 检查子项名称, 检查所见, 检查结论,...
    ## dttm (18): 检查时间, 胸部CT检查-检查时间, 头部CT检查-检查时间, 腹部CT检查-检查时间, 盆腔CT检查-检查时间, 泌尿系CT检查...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ExamReport_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/检查.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 38299 Columns: 28
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 检查分类, 检查子项名称, 检查所见, 检查结论, 检查编号...
    ## dbl   (1): 病案号
    ## dttm (18): 检查时间, 胸部CT检查-检查时间, 头部CT检查-检查时间, 腹部CT检查-检查时间, 盆腔CT检查-检查时间, 泌尿系CT检查...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ExamReport_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/检查.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 24214 Columns: 28
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (9): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 检查分类, 检查子项名称, 检查所见, 检查结论, 检查编号...
    ## dbl   (1): 病案号
    ## dttm (18): 检查时间, 胸部CT检查-检查时间, 头部CT检查-检查时间, 腹部CT检查-检查时间, 盆腔CT检查-检查时间, 泌尿系CT检查...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ExamReport_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/检查.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 14654 Columns: 28
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 检查分类, 检查子项名称, 检查所见, 检查结论,...
    ## dttm (18): 检查时间, 胸部CT检查-检查时间, 头部CT检查-检查时间, 腹部CT检查-检查时间, 盆腔CT检查-检查时间, 泌尿系CT检查...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ExamReport <- rbind(
  ExamReport_EICU1,ExamReport_EICU2,
  ExamReport_ICU1,ExamReport_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    "ExamReport_Category" =`检查分类`,
    "ExamReport_item_DESC" = `检查子项名称`,
    "ExamReport_DESC" = `检查所见`,
    "ExamReport_finding" =`检查结论`, 
    "ExamReport_time" = `胸部CT检查-检查时间`
  ) %>% 
  mutate(
    ExamReport_Category = recode(
      ExamReport_Category,
      "X线检查" = "X ray",
      "超声" = "Ultrasound",
      "内镜" = "Endoscopy"
    )
  )
  
table(ExamReport$Hospital_ID%in%EMR$Hospital_ID)
```

    ## 
    ##  TRUE 
    ## 93466

``` r
write(
  unique(ExamReport$ExamReport_item_DESC),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/ExamReport_item.csv"
  )
ExamReport_item_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/ExamReport_item.xlsx",
  col_names = c("ExamReport_item_Chin","ExamReport_item_Eng")
)
ExamReport <- ExamReport %>% 
  left_join(
    ExamReport_item_Chin2Eng,
    by = c("ExamReport_item_DESC" = "ExamReport_item_Chin")
  ) %>% 
   left_join(ZeroTime) %>% 
  mutate(ExamReport_time = as.duration(HospitalAdmissionTime %--% ExamReport_time)/ddays(1)
         ) %>% 
  select(-c(ExamReport_item_DESC,HospitalAdmissionTime))
```

    ## Joining, by = "Hospital_ID"

``` r
table(ExamReport$Hospital_ID%in%EMR$Hospital_ID)
```

    ## 
    ##  TRUE 
    ## 93466

``` r
write.csv(
  ExamReport,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/ExamReport.csv"
)
```

\#检验-微生物培养

``` r
MicrobiologyCulture_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/检验-微生物培养.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 52049 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 微生物培养细菌名称, 微生物培养细菌名称（医渡标准...
    ## lgl   (1): 检验子项目名称定量值
    ## dttm  (1): 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
MicrobiologyCulture_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/检验-微生物培养.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 73958 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 微生物培养细菌名称, 微生物培养细菌名称（医渡标准化）, 微...
    ## dbl   (1): 病案号
    ## lgl   (1): 检验子项目名称定量值
    ## dttm  (1): 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
MicrobiologyCulture_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/检验-微生物培养.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 54215 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 微生物培养细菌名称, 微生物培养细菌名称（医渡标准化）, 微...
    ## dbl   (1): 病案号
    ## lgl   (1): 检验子项目名称定量值
    ## dttm  (1): 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
MicrobiologyCulture_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/检验-微生物培养.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 62773 Columns: 14
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 微生物培养细菌名称, 微生物培养细菌名称（医渡标准...
    ## lgl   (1): 检验子项目名称定量值
    ## dttm  (1): 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
MicrobiologyCulture <- rbind(
  MicrobiologyCulture_EICU1,MicrobiologyCulture_EICU2,
  MicrobiologyCulture_ICU1,MicrobiologyCulture_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    "MicrobiologyCulture_Category" =`检验套餐名称`,
    "MicrobiologyCulture_DESC" = `微生物培养细菌名称`,
    "MicrobiologyCulture_finding" =`微生物培养结果`, 
    "MicrobiologyCulture_time" = `检验时间`,
    "MicrobiologyCulture_sample" =`检验标本`
  ) 
write(
  unique(MicrobiologyCulture$MicrobiologyCulture_sample),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/MicrobiologyCulture_sample.csv"
  )
MicrobiologyCulture_sample_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/MicrobiologyCulture_sample.xlsx",
  col_names = c("MicrobiologyCulture_sample_Chin","MicrobiologyCulture_sample_Eng")
)
write(
  unique(MicrobiologyCulture$MicrobiologyCulture_Category),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/MicrobiologyCulture_Category.csv"
  )
MicrobiologyCulture_Category_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/MicrobiologyCulture_Category.xlsx",
  col_names = c("MicrobiologyCulture_Category_Chin","MicrobiologyCulture_Category_Eng")
)
write(
  unique(unique(MicrobiologyCulture$MicrobiologyCulture_DESC)),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/MicrobiologyCulture_DESC.csv"
  )
MicrobiologyCulture_DESC_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/MicrobiologyCulture_DESC.xlsx",
  col_names = c("MicrobiologyCulture_DESC_Chin","MicrobiologyCulture_DESC_Eng")
)
MicrobiologyCulture <- MicrobiologyCulture %>% 
  left_join(MicrobiologyCulture_sample_Chin2Eng,
            by = c("MicrobiologyCulture_sample"="MicrobiologyCulture_sample_Chin")) %>% 
  left_join(MicrobiologyCulture_Category_Chin2Eng,
            by = c("MicrobiologyCulture_Category"="MicrobiologyCulture_Category_Chin")
            ) %>% 
  left_join(MicrobiologyCulture_DESC_Chin2Eng,
            by=c("MicrobiologyCulture_DESC"="MicrobiologyCulture_DESC_Chin")) %>% 
  left_join(ZeroTime) %>% 
  mutate(MicrobiologyCulture_time = as.duration(HospitalAdmissionTime %--% MicrobiologyCulture_time)/ddays(1)
         ) %>% 
  select(-c(HospitalAdmissionTime,MicrobiologyCulture_Category,MicrobiologyCulture_DESC,MicrobiologyCulture_sample))
```

    ## Joining, by = "Hospital_ID"

``` r
write.csv(
  MicrobiologyCulture,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/MicrobiologyCulture.csv"
)
```

\#检验-药敏试验

``` r
DrugSens_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组1/检验-药敏试验.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 256822 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 药物中文名称, 药物编码, 敏感度, 最低抑菌浓度...
    ## dttm  (1): 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
DrugSens_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组2/检验-药敏试验.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 182155 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (11): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 药物中文名称, 药物编码, 敏感度, 最低抑菌浓度, 药敏检...
    ## dbl   (1): 病案号
    ## dttm  (1): 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
DrugSens_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组3/检验-药敏试验.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 128008 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (10): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 药物中文名称, 敏感度, 最低抑菌浓度, 药敏检验结果, 检...
    ## dbl   (1): 病案号
    ## lgl   (1): 药物编码
    ## dttm  (1): 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
DrugSens_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/研究组4/检验-药敏试验.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 167039 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (12): patient_SN, 病案号, 门诊号, 住院号, 就诊标识（医渡云计算）, 药物中文名称, 药物编码, 敏感度, 最低抑菌浓度...
    ## dttm  (1): 检验时间
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
DrugSens <- rbind(
  DrugSens_EICU1,DrugSens_EICU2,
  DrugSens_ICU1,DrugSens_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`,
    Drug_name = `药物中文名称`,
    Drug_Code = `药物编码`,
    DrugSens_result = 敏感度,
    MIC = `最低抑菌浓度`,
    DrugSens_Microbiology = `药敏检验结果`,
    DrugSens_Category = 检验套餐名称,
    DrugSens_sample = 检验标本,
    DrugSens_time = 检验时间
  ) %>% 
  mutate(
    DrugSens_result = recode(
      DrugSens_result,
      不敏感 = "Insensitive",
      中间 = "Intermediate",
      剂量依赖性敏感 = "Dose-dependent sensitive",
      敏感 = "Sensitive",
      耐药 = "Resistance",
      阳性 = "Positive",
      阴性 = "Negative"
      )
  )
write(
  unique(DrugSens$Drug_name),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Drug_name.csv"
  )
Drug_name_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/Drug_name.xlsx",
  col_names = c("Drug_name_Chin","Drug_name_Eng")
)
write(
  unique(DrugSens$DrugSens_Microbiology),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DrugSens_Microbiology.csv"
  )
DrugSens_Microbiology_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DrugSens_Microbiology.xlsx",
  col_names = c("DrugSens_Microbiology_Chin","DrugSens_Microbiology_Eng")
)
write(
  unique(DrugSens$DrugSens_Category),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DrugSens_Category.csv"
  )
DrugSens_Category_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DrugSens_Category.xlsx",
  col_names = c("DrugSens_Category_Chin","DrugSens_Category_Eng")
)
write(
  unique(DrugSens$DrugSens_sample),
  file = "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DrugSens_sample.csv"
  )
DrugSens_sample_Chin2Eng <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DrugSens_sample.xlsx",
  col_names = c("DrugSens_sample_Chin","DrugSens_sample_Eng")
)
DrugSens <- DrugSens %>% 
  left_join(
    Drug_name_Chin2Eng, by = c("Drug_name"="Drug_name_Chin")
  ) %>% 
  left_join(
    DrugSens_Microbiology_Chin2Eng, by = c("DrugSens_Microbiology"="DrugSens_Microbiology_Chin")
  ) %>%
  left_join(
    DrugSens_Category_Chin2Eng, by = c("DrugSens_Category"="DrugSens_Category_Chin")
  ) %>%
  left_join(
    DrugSens_sample_Chin2Eng, by = c("DrugSens_sample"="DrugSens_sample_Chin")
  ) %>%
  left_join(ZeroTime) %>% 
  mutate(DrugSens_time = as.duration(HospitalAdmissionTime %--% DrugSens_time)/ddays(1)
         ) %>% 
  select(-c(HospitalAdmissionTime,Drug_name,DrugSens_Microbiology,DrugSens_Category,DrugSens_sample))
```

    ## Joining, by = "Hospital_ID"

``` r
write.csv(
  DrugSens,
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/DrugSens.csv"
)
```

\#评分量表

``` r
ScaleMetric_EICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU20120101-20191231/评分量表.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 361 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 评分记录来源, 评分名称, 评分时间, 总分, 评分项目名称,...
    ## dbl  (1): 病案号
    ## lgl  (1): 评分项目值单位
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ScaleMetric_EICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU20200101-20220518/评分量表.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 850 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 评分记录来源, 评分名称, 评分时间, 总分, 评分项目名称,...
    ## dbl  (1): 病案号
    ## lgl  (1): 评分项目值单位
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ScaleMetric_ICU1 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/ICU20120101-20191231/评分量表.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 1547 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (12): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 评分记录来源, 评分名称, 评分时间, 总分, 评分项目名称,...
    ## dbl  (1): 病案号
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ScaleMetric_ICU2 <- readr::read_csv(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/ICU20200101-20220519/评分量表.csv",
  locale=readr::locale(encoding="GB18030")
)
```

    ## Rows: 2355 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (11): patient_SN, 门诊号, 住院号, 就诊标识（医渡云计算）, 评分记录来源, 评分名称, 评分时间, 总分, 评分项目名称,...
    ## dbl  (1): 病案号
    ## lgl  (1): 评分项目值单位
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
ScaleMetric <- rbind(
  ScaleMetric_EICU1,ScaleMetric_EICU2,
  ScaleMetric_ICU1,ScaleMetric_ICU2
) %>% 
  select(
    patient_SN,
    "Hospital_ID" = `就诊标识（医渡云计算）`
  )
```

# 护理数据单子

``` r
EICU1 <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU数据/科研数据2017年.xlsx")
EICU2 <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU数据/科研数据2018年.xlsx")
EICU3 <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU数据/科研数据2019年.xlsx")
EICU4 <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU数据/科研数据2020年3月(新版本上线).xlsx")
EICU5 <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU数据/科研数据2021.xlsx")
EICU6 <- readxl::read_xlsx(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/EICU数据/科研数据2022年5月.xlsx")
EICU_nursingChart <- rbind(
  EICU1,EICU2,EICU3,EICU4,EICU5,EICU6
) %>% 
  mutate(`病案号` = str_remove_all(`病案号`,"\\*"))
  
table(unique(EICU_nursingChart$病案号)%in% NursingChartMatch$MedID)
```

    ## 
    ## FALSE  TRUE 
    ##   275   827

\#表格基本描述

``` r
library(reproducible)
#size for each table
TableName <- list.files(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/"
)
TablePath <- list.files(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/DataTable/",
  full.names = T
)
MD5_hashes <- tools::md5sum(TablePath)
FileSize <- file.info(TablePath)$size
TableDesc <- data.frame(
  Tables = TableName,
  MD5_hashes = MD5_hashes,
  Size = FileSize,
  Description = c(
    "Diagnosis",
    "Sensitivity to antibiotics for cultured bacteria",
    "Electronic medical records for each hsopital admission",
    'Examination report including CT, ultrasound and MRI',
    "intrahospital transfer events",
    "Laboratory findings",
    "Medication events",
    "Medical order",
    "Microbiology cuture",
    "Vital signs"
  )
)
rownames(TableDesc) <- NULL
print(TableDesc,quote = T)
```

    ##                       Tables                         MD5_hashes         Size
    ## 1            "Diagnosis.csv" "3b7ca8b430b16d9ebbd1317cb06cc87b" "  25582236"
    ## 2             "DrugSens.csv" "2f79976765464593b8eed552221c6359" " 136436217"
    ## 3                  "EMR.csv" "5c6048462b1dc6d44a47687fb34bbc65" "  13730602"
    ## 4           "ExamReport.csv" "f771ad05ec45b2b65105744b2c29907f" "  52649246"
    ## 5     "HospitalTransfer.csv" "168fef171980a7cacbe2ed79d1dd63ba" "   1441407"
    ## 6                  "Lab.csv" "4939ebb2155bbcfaff37da8e78f8cc4a" "1828953993"
    ## 7           "Medication.csv" "4e9d16531c6cfb2a7d9aafc21f465e16" " 277782777"
    ## 8             "MedOrder.csv" "297d4e9f5e7e9ba8c3dca5005b8d7684" " 207348204"
    ## 9  "MicrobiologyCulture.csv" "af996f30325ec74eefbf7a25d6680473" "  39362619"
    ## 10           "VitalSign.csv" "5224b7450833a2ac441d16b459502f32" " 607364251"
    ##                                                 Description
    ## 1                                               "Diagnosis"
    ## 2        "Sensitivity to antibiotics for cultured bacteria"
    ## 3  "Electronic medical records for each hsopital admission"
    ## 4     "Examination report including CT, ultrasound and MRI"
    ## 5                           "intrahospital transfer events"
    ## 6                                     "Laboratory findings"
    ## 7                                       "Medication events"
    ## 8                                           "Medical order"
    ## 9                                     "Microbiology cuture"
    ## 10                                            "Vital signs"

``` r
#不同表格的内涵展示
EMR_Tab <- data.frame(
  "TableName" = rep("EMR",length(names(EMR))),
  "Varibales" = names(EMR),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Sex",
                    "Chief Complain for patients who discharged within 24 hours after hospital admission",
                    "Admission Status for patients who discharged within 24 hours after hospital admission",
                    "Chief Complain for patients who died within 24 hours after hospital admission",
                    "Admission Status for patients who died within 24 hours after hospital admission",
                    "Chief Complain in Chinese",
                    "Medical history in text",
                    "Past history/comorbidities",
                    "Status On Discharge",
                    "Diagnosis On Death",
                    "Status On Discharge described in text",
                    "Discharge time relative to hospital admission time as the time zero in days",
                    "Days of Hospital Stay",
                    "Chief Complain in English",
                    "Age in category")
) 
Diagnosis_Tab <- data.frame(
  "TableName" = rep("Diagnosis",length(names(Diagnosis))),
  "Variables" = names(Diagnosis),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Description of diagnosis in free text",
                    "ICD-10 code",
                    "ICD-10 name for the diagnosis",
                    "Time for making the diagnosis relative to hospital admission time as the time zero in days")
) 
DrugSens_Tab <- data.frame(
  "TableName" = rep("DrugSens",length(names(DrugSens))),
  "Variables" = names(DrugSens),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Code of the drug for sensitivity analysis",
                    "Results for Drug Sensitivity test",
                    "Minimum inhibitory concentration",
                    "Time for the results relative to hospital admission time as the time zero in days",
                    "Name of the tested drug",
                    "Microorganism for testing",
                    "Category for the test",
                    "Sample name")
) 
ExamReport_Tab <- data.frame(
  "TableName" = rep("ExamReport",length(names(ExamReport))),
  "Variables" = names(ExamReport),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Category of examination",
                    "Description of the examination in free form text",
                    "Result finding",
                    "Time for the examination results relative to hospital admission time as the time zero in days",
                    "Name of the Examination")
) 
HospitalTransfer_Tab <- data.frame(
  "TableName" = rep("HospitalTransfer",length(names(HospitalTransfer))),
  "Variables" = names(HospitalTransfer),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "time of transfer in in days relative to hospital admission",
                    "time of transfer out in days relative to hospital admission",
                    "department of transfer to",
                    "department of transfer from")
) 
Lab_Tab <- data.frame(
  "TableName" = rep("Lab",length(names(Lab))),
  "Variables" = names(Lab),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Category of lab item",
                    "Time of lab in days relative to hospital admission",
                    "Results of the lab finding",
                    "Unit of measurement",
                    "Sample collection time in days relative to hospital admission",
                    "Name of lab item",
                    "Sample name")
) 
Medication_Tab <- data.frame(
  "TableName" = rep("Medication",length(names(Medication))),
  "Variables" = names(Medication),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Category of medication",
                    "Single dose",
                    "Frequency of administration",
                    "Unit of measurement",
                    "Start time of medication in days relative to hospital admission",
                    "Stop time of medication in days relative to hospital admission",
                    "Route of administration",
                    "Medication name in text")
) 
MedOrder_Tab <- data.frame(
  "TableName" = rep("MedOrder",length(names(MedOrder))),
  "Variables" = names(MedOrder),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Type of medical order: regular or stat",
                    "Description of medical order in free text",
                    "Start time of medication in days relative to hospital admission",
                    "Stop time of medication in days relative to hospital admission")
) 
MicrobiologyCulture_Tab <- data.frame(
  "TableName" = rep("MicrobiologyCulture",length(names(MicrobiologyCulture))),
  "Variables" = names(MicrobiologyCulture),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Microbiology Culture finding",
                    "Microbiology Culture time in days relative to hospital admission",
                    "Microbiology Culture sample",
                    "Microbiology Culture Category",
                    "Description of Microbiology Culture")
) 
VitalSign_Tab <- data.frame(
  "TableName" = rep("VitalSign",length(names(VitalSign))),
  "Variables" = names(VitalSign),
  "Explanation" = c("Patient series number: unique to each individual subject",
                    "unique to each hospital admission",
                    "Vital Sign Description",
                    "Vital Sign value",
                    "Vital Sign unit of measurement",
                    "Vital Sign measurement time in days relative to hospital admission")
) 
```

\#General description of patient characteristics

``` r
library(CBCgrps)
```

    ## Loading required package: nortest

``` r
tab1 <- twogrps(
  EMR %>% 
    mutate(StatusOnDischarge = recode(StatusOnDischarge,
                                      Improved="Cured",
                                      `Discharge to rehabilitation center`="Recovered",
                                      `Recovered` = "Cured",
                                      "Recovered" = "Cured",
                                      `Discharge against medical order`="Not cured",
                                      Readmission = "Not cured",
                                      Others = "Unknown"),
           StatusOnDischarge = if_else(StatusOnDischarge=="Recovered","Cured",StatusOnDischarge)
           ) %>% 
    as.data.frame(),
  gvar= "Sex",
  varlist = c("Age_cut","DaysHospitalStay","StatusOnDischarge")
)
#入院的时间趋势
library(lubridate)
library(ggstatsplot)
```

    ## Warning: package 'ggstatsplot' was built under R version 4.1.2

    ## You can cite this package as:
    ##      Patil, I. (2021). Visualizations with statistical details: The 'ggstatsplot' approach.
    ##      Journal of Open Source Software, 6(61), 3167, doi:10.21105/joss.03167

``` r
plotTimeSeries <- EMR_add %>% 
  mutate(Month = round_date(HospitalAdmissionTime,unit = "quarter")) %>% 
  count(Month,Sex) %>% 
  ggplot(aes(x = Month, y = n)) + 
  geom_area(aes(color = Sex, fill = Sex), 
            alpha = 0.5, position = position_dodge(0.8)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))+
  theme_minimal()+
  labs(x= "Resolution in Quarter",y = "Number of admissions")
pdf(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/plotTimeSeries.pdf",
  width = 6,height = 6
)  
plotTimeSeries
dev.off()
```

    ## quartz_off_screen 
    ##                 2

``` r
#不同患者住院时长分布
Plot_LOS <- EMR %>% 
  filter( DaysHospitalStay < 60) %>% 
  ggstatsplot::grouped_gghistostats(
    x = DaysHospitalStay,
    xlab = "Days of hospital stay",
     grouping.var = Sex,
    plotgrid.args = list(nrow = 2),
    bin.args = list(color = "black", fill = "red", alpha = 0.7)
  )
pdf(
  "/Users/zhangzhongheng/Documents/2022/浙江省人民医院数据库构建/LOS_Sex.pdf",
  width = 6,height = 6
)  
Plot_LOS
dev.off()
```

    ## quartz_off_screen 
    ##                 2
