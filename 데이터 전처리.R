# 데이터 전처리를 다룬 코드입니다.
# 1. 패키지 로드
library(tidyverse)
library(corrplot)
library(car) #

# 2. 데이터 로드
pitching <- read.csv("pitching.csv", stringsAsFactors = FALSE)
batting <- read.csv("batting.csv", stringsAsFactors = FALSE)
team <- read.csv("team.csv", stringsAsFactors = FALSE)

# 3. 팀-연도별 집계 (batting)
batting_team <- batting %>%
  group_by(year, team_id) %>%
  summarise(
    total_h = sum(h, na.rm = TRUE),
    total_double = sum(double, na.rm = TRUE),
    total_triple = sum(triple, na.rm = TRUE),
    total_hr = sum(hr, na.rm = TRUE),
    total_bb = sum(bb, na.rm = TRUE),
    total_so = sum(so, na.rm = TRUE),
    total_sb = sum(sb, na.rm = TRUE),
    .groups = "drop"  # 그룹 해제
  )

# 4. 팀-연도별 집계 (pitching)
pitching_team <- pitching %>%
  group_by(year, team_id) %>%
  summarise(
    pitch_wins = sum(w, na.rm = TRUE),
    pitch_so = sum(so, na.rm = TRUE),
    pitch_bb = sum(bb, na.rm = TRUE),
    pitch_hr = sum(hr, na.rm = TRUE),
    avg_era = mean(era, na.rm = TRUE),
    total_ipouts = sum(ipouts, na.rm = TRUE),
    .groups = "drop"  # 그룹 해제
  )

# 5. 병합
merged <- team %>%
  left_join(batting_team, by = c("year", "team_id")) %>%
  left_join(pitching_team, by = c("year", "team_id"))

# 6. 결측치 제거 + 파생변수 생성 (BA, OBP, SLG, OPS, RD)
merged <- merged %>%
  drop_na(w) %>%  # 승리 수 없는 행 제거
  mutate(
    # 득점 - 실점 차이
    rd = r - ra,
    
    # 타율
    ba = total_h / ab,
    
    # 출루율 (간단 버전)
    obp = (total_h + total_bb) / (ab + total_bb),
    
    # 장타율
    slg = (total_h + total_double + 2*total_triple + 3*total_hr) / ab,
    
    # OPS = OBP + SLG
    ops = obp + slg
  )

# 7. 2000~2015 필터링 (모델링용)
merged_2000 <- merged %>%
  filter(year >= 2000 & year <= 2015)

# 8. 결과 확인 (예: 2015년 상위 5팀)
merged_2000 %>%
  filter(year == 2015) %>%
  select(year, team_id, name, w, rd, ba, obp, slg, ops) %>%
  arrange(desc(w)) %>%
  head(5)

merged_2000

# merged_2000을 CSV로 저장
write.csv(merged_2000, 
          file = "mlb_team_stats_2000_2015.csv", 
          row.names = FALSE)