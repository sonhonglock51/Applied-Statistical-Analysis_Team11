# 필요한 패키지 로드
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

# 데이터 파일 불러오기 (업로드된 파일 사용)
data <- read.csv("mlb_team_stats_2000_2015(SB% 추가, total_hr 제거본).csv")

# 🚨 예측 대상 변수 'w'를 이진(Binary) 변수로 변환 (기준 수정): 
# 승리 횟수(w)가 81보다 크면(즉, 82승 이상) "Win", 아니면 "Loss"
data <- data %>%
  mutate(
    Win_Loss = factor(
      # 기준 변경: w > 81 (82승 이상)
      ifelse(w > 81, "Win", "Loss"), 
      levels = c("Loss", "Win")
    )
  )

# 사용할 변수 선택
model_data <- data %>%
  select(ra, slg, total_bb, pitch_so, pitch_bb, sb, Win_Loss)

# NA 값 확인 및 제거
model_data <- na.omit(model_data)

# 데이터 분할: 훈련 세트(70%)와 테스트 세트(30%)
set.seed(42) 
train_index <- createDataPartition(model_data$Win_Loss, p = 0.7, list = FALSE)
train_set <- model_data[train_index, ]
test_set <- model_data[-train_index, ]

cat("데이터 준비 완료. \n")
cat("새로운 승리 기준 (w > 81) 적용. \n")
cat("훈련 세트 크기:", nrow(train_set), "\n")
cat("테스트 세트 크기:", nrow(test_set), "\n")

# 의사결정 나무 모델 훈련
tree_model_new <- rpart(
  Win_Loss ~ ra + slg + total_bb + pitch_so + pitch_bb + sb,
  data = train_set,
  method = "class",
  control = rpart.control(cp = 0.001) 
)

cat("의사결정 나무 모델 훈련 완료.\n")

# KBO 데이터 세트에 대한 예측
kbo <- read.csv("KBO예측데이터.csv")
kbo$Win_Loss <- factor(kbo$Win_Loss, levels = c("Loss", "Win"))
prediction_kbo <- predict(tree_model_new,newdata = kbo,type = 'class')

# 혼동 행렬
confusion_matrix_new <- confusionMatrix(predictions_kbo, kbo$Win_Loss)

cat("\n### 의사결정 나무 모델 성능 분석 ###\n")
print(confusion_matrix_new)

cat("\n### 🔑 주요 변수 중요도 (Variable Importance) ###\n")
print(tree_model_new$variable.importance)

# 정확도(Accuracy) 추출
accuracy_new <- confusion_matrix_new$overall['Accuracy']
cat("\n👉 최종 모델 정확도 (Accuracy, w > 81 기준):", round(accuracy_new * 100, 2), "%\n")
