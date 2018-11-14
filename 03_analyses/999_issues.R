smoking <- smoking %>% mutate(pow = 1)
smoking_trt <- smoking$treatments
smoking_dat <- smoking$data.ab %>% mutate(pow = 1)

net <- mtc.network(smoking_dat, smoking_trt, studies = smoking_dat[,c(1,5)])
model <- mtc.model(net, type="consistency", powerAdjust = "pow")

# Load pre-generated samples instead of runing the model:
results <- mtc.run(model, thin=10)


results <- dget(system.file("extdata/luades-smoking.samples.gz", package="gemtc"))


write(results$model$code, file  = "test.txt")


results$model$data

