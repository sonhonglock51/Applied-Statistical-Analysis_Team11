system('git config --global user.email "rlarkdms533@gmail.com"')
system('git config --global user.name "sonhonglock51"')
system('git config --global --list')
library(tidyverse)
library(car)

mlb_stats <- read.csv("mlb_team_stats_2000_2015.csv", stringsAsFactors = FALSE)
mlb_stats <- na.omit(mlb_stats)

# 2. 숫자형 변수만 선택
# is.numeric으로 숫자형 변수만 필터링합니다.
numeric_vars <- mlb_stats %>%
  select(where(is.numeric))

# 3. 상관 행렬 계산
correlation_matrix <- cor(numeric_vars)

# 4. 'w'와의 상관계수 추출 및 정렬
w_correlations <- correlation_matrix["w", ]

correlation_df_all <- as.data.frame(w_correlations) %>%
  rownames_to_column(var = "Variable") %>%
  rename(Correlation = w_correlations) %>%
  # w 자기 자신과의 상관관계(r=1)와 연도(year)는 제외
  filter(Variable != "w" & Variable != "year") %>%
  arrange(desc(Correlation))

cat("--- 승리(w)와 모든 숫자형 변수 간의 상관관계 (내림차순) ---\n")
print(correlation_df_all)

# 2. 초기 독립 변수 후보 목록 생성 (유사 지표 3개 제외)
# rd, pitch_wins, l을 제외하고, R/RA/효율 지표를 중심으로 초기 모델 변수 설정
initial_candidate_vars <- c("r", "ra", "ops", "slg", "avg_era", "total_hr", "total_bb", "pitch_so", "pitch_bb")

# 3. 초기 회귀 모델 구축
# 이 모델은 VIF를 확인하기 위한 임시 모델입니다.

initial_model <- lm(
  as.formula(paste("w ~", paste(initial_candidate_vars, collapse = " + "))),
  data = mlb_stats
)

cat("\n--- [초기 VIF 진단] (VIF > 5 이상 제거 대상) ---\n")
vif_initial <- vif(initial_model)
print(vif_initial)

# ops, r, avg_era 제거 후 모델
vars_step1 <- c("ra", "slg", "total_hr", "total_bb", "pitch_so", "pitch_bb")
model_step1 <- lm(
  as.formula(paste("w ~", paste(vars_step1, collapse = " + "))),
  data = mlb_stats
)

cat("\n--- [1차 제거 후 VIF 진단] (r, ops, avg_era 제거) ---\n")
vif_step1 <- vif(model_step1)
print(vif_step1)