library(datacampr)

soft_launches <- tbl_main_course_state_logs() %>%
  filter(new_state == "soft_launch") %>%
  select(course_id, soft_launch_at = created_at)

hard_launches <- tbl_main_course_state_logs() %>%
  filter(new_state == "live") %>%
  select(course_id, live_at = created_at)

# inner first-first
test_results <- after_join(soft_launches,
                           hard_launches,
                           by_user = "course_id",
                           by_time = c("soft_launch_at" = "live_at")) %>%
  collect()
# Are there any NAs in the y col?
anyNA(test_results$live_at)

# Is the left col always the first by course_id?
first_soft_launches <- soft_launches %>%
  group_by(course_id) %>%
  filter(soft_launch_at == min(soft_launch_at, na.rm = T)) %>%
  ungroup() %>%
  collect() %>%
  rename(first_soft_launch_at = soft_launch_at)

test_results %>%
  left_join(first_soft_launches, by = "course_id") %>%
  count(first_soft_launch_at == soft_launch_at)

