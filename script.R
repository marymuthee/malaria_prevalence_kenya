setwd("C:/Users/MARY/Desktop/Masters/Studies/GeoHumanitarian Action/Data")
getwd()
dir()

library(malariaAtlas)
library(terra)

#check whether there is occurence data for Kenya
isAvailable_pr(country = "Kenya")
isAvailable_vec(country = "Kenya")

#downlaod raster layer for kenya
KE_shp <- getShp(country = "Kenya", admin_level = "admin0")
KE_Pf <- getRaster(surface = "Plasmodium falciparum PR2-10", 
                   shp = KE_shp)
plot(KE_Pf)

# -Randomly sample points 
# na.rm=TRUE skips any NoData cells automatically
n_total <- 50  # total random points to sample
samples <- spatSample(KE_Pf, size = n_total, method = "random",
                      as.points = TRUE, values = TRUE, na.rm = TRUE)

#  Convert to dataframe 
df <- as.data.frame(samples, geom = "XY")  
# this automatically creates columns: x, y, and the PfPR value
names(df) <- c("pfpr", "lon", "lat")    

#  Classify PfPR into categories ----
high_thr <- 0.10
med_thr  <- 0.04

df$category <- with(df, ifelse(pfpr >= high_thr, "High",
                               ifelse(pfpr >= med_thr, "Medium", "Low")))

# Balance classes 
samples_per_class <- 50 
set.seed(42)

subset_class <- function(cat) {
  d <- subset(df, category == cat)
  if (nrow(d) > samples_per_class) {
    d <- d[sample(seq_len(nrow(d)), samples_per_class), ]
  }
  d
}

df_high   <- subset_class("High")
df_medium <- subset_class("Medium")
df_low    <- subset_class("Low")

df_sites <- rbind(df_high, df_medium, df_low)

#   Add name + export                  
df_sites$name <- ave(df_sites$category, df_sites$category,
                     FUN = function(x) paste0(x, "_site_", seq_along(x)))

out_csv <- "malaria_sites.csv"  
write.csv(df_sites[, c("name", "category", "lon", "lat", "pfpr")], out_csv, row.names = FALSE)

cat("Counts per class:\n"); print(table(df_sites$category))

                     
cat("\nPfPR summary by class:\n"); print(aggregate(pfpr ~ category, df_sites, summary))
cat("\n✅ Wrote", nrow(df_sites), "rows to", out_csv, "\n")

