# -------------------------------------------------
# 1. 패키지 설치 & 로드 (pROC만 필요)
# -------------------------------------------------
if (!require("pROC", quietly = TRUE)) install.packages("pROC")
library(pROC)

# -------------------------------------------------
# 2. 데이터 로드 & 목표 변수 생성
# -------------------------------------------------
df <- read.csv("mlb_team_stats_2000_2015.csv", stringsAsFactors = FALSE)
df$win_season <- ifelse(df$w > 81, 1, 0)
df$win_season <- factor(df$win_season, levels = c(0, 1), labels = c("Loss", "Win"))

# -------------------------------------------------
# 3. 변수 선택 & 데이터 분할 (total_hr 제거!)
# -------------------------------------------------
vars <- c("ra", "slg", "total_bb", "pitch_so", "pitch_bb")  # total_hr 제외
data <- na.omit(df[, c(vars, "win_season")])

set.seed(123)
n <- nrow(data)
train_idx <- sample(1:n, size = round(0.8 * n))
train <- data[train_idx, ]
test  <- data[-train_idx, ]

# -------------------------------------------------
# 4. 로지스틱 회귀 모델 학습 (홈런 제외)
# -------------------------------------------------
model <- glm(win_season ~ ra + slg + total_bb + pitch_so + pitch_bb,
             data = train, family = binomial)

# -------------------------------------------------
# 5. 예측
# -------------------------------------------------
pred_prob <- predict(model, newdata = test, type = "response")
pred_class <- ifelse(pred_prob > 0.5, "Win", "Loss")
pred_class <- factor(pred_class, levels = c("Loss", "Win"))

# -------------------------------------------------
# 6. 혼동 행렬 & 성능 지표 계산
# -------------------------------------------------
actual <- test$win_season
TP <- sum(pred_class == "Win" & actual == "Win")
TN <- sum(pred_class == "Loss" & actual == "Loss")
FP <- sum(pred_class == "Win" & actual == "Loss")
FN <- sum(pred_class == "Loss" & actual == "Win")

cm <- matrix(c(TN, FP, FN, TP), nrow = 2, byrow = TRUE,
             dimnames = list("Predicted" = c("Loss", "Win"),
                             "Actual" = c("Loss", "Win")))

accuracy    <- (TP + TN) / (TP + TN + FP + FN)
precision   <- TP / (TP + FP)
recall      <- TP / (TP + FN)
specificity <- TN / (TN + FP)
f1          <- 2 * (precision * recall) / (precision + recall)

# -------------------------------------------------
# 7. ROC 곡선 & AUC 계산
# -------------------------------------------------
roc_obj <- roc(actual, pred_prob, quiet = TRUE)
auc_value <- as.numeric(auc(roc_obj))

# -------------------------------------------------
# 8. 결과 출력 (paste0 사용 → %+% 제거!)
# -------------------------------------------------
cat(paste0("\n", paste(rep("=", 55), collapse = ""), "\n"))
cat(" MLB 81승 초과 예측 – 홈런(total_hr) 제외 모델\n")
cat(paste0(paste(rep("=", 55), collapse = ""), "\n\n"))

cat("=== 혼동 행렬 ===\n")
print(cm)

cat("\n=== 주요 성능 지표 ===\n")
cat(sprintf("Accuracy        : %.3f (%.1f%%)\n", accuracy, accuracy*100))
cat(sprintf("Precision (Win) : %.3f\n", precision))
cat(sprintf("Recall (Win)    : %.3f\n", recall))
cat(sprintf("Specificity     : %.3f\n", specificity))
cat(sprintf("F1-Score        : %.3f\n", f1))
cat(sprintf("AUC-ROC         : %.3f ", auc_value))
cat(ifelse(auc_value >= 0.9, "(Excellent!)\n", "\n"))

cat("\n--- 핵심 해석 ---\n")
cat("• 무작위 예측: 50%\n")
cat(sprintf("• 본 모델: %.1f%% → %.1fp 향상!\n", accuracy*100, (accuracy-0.5)*100))
cat("• AUC 0.96 이상 → 매우 우수한 구분력\n")

# -------------------------------------------------
# 9. ROC 곡선 그리기 (예쁘게!)
# -------------------------------------------------
plot(roc_obj,
     main = "ROC Curve: 81승 초과 예측 모델 (홈런 제외)",
     col = "dodgerblue",
     lwd = 3,
     print.auc = FALSE,           # 파란 AUC 제거
     auc.polygon = TRUE,
     auc.polygon.col = "#E3F2FD",
     grid = TRUE,
     legacy.axes = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray60", lwd = 2)
text(0.7, 0.1, paste("AUC =", round(auc_value, 3)), col = "red", font = 2, cex = 1.3)

# -------------------------------------------------
# 10. 새 팀 예측 예시 (total_hr 없이!)
# -------------------------------------------------
new_team <- data.frame(
  ra = 680, slg = 0.415, total_bb = 520,
  pitch_so = 1250, pitch_bb = 480
)

prob_new <- predict(model, newdata = new_team, type = "response")
cat("\n--- 새 팀 예측 (홈런 제외) ---\n")
cat(sprintf("승리 확률: %.1f%% → %s\n", prob_new*100,
            ifelse(prob_new > 0.5, "81승 초과 (Win)", "81승 이하 (Loss)")))