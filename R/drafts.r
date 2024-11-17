# world_raster_url <- '/vsicurl/https://dataforgood-fb-data.s3.us-east-1.amazonaws.com/hrsl-cogs/hrsl_general/hrsl_general-latest.vrt'
# world_raster <- rast(world_raster_url)

# # summary(wordl_raster)

# # Create a lower-resolution version
# tictoc::tic()
# world_raster_coarse <- terra::project(world_raster, res = 1, method = "bilinear")
# tictoc::toc()

# describe(world_raster)

# world_raster_coarse

# # Plot the lower-resolution raster
# plot(world_raster_coarse)
