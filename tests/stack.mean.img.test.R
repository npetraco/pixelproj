

stk <- thresh.img.obj(image_read(pth), type = "individual.mean")
mean_img <- mean(stk, target="new")
mean_img[,,1]
stk[[1]][,,1]

mean_img2 <- thresh.img.obj(mean_img, type = "individual.mean")
mean_img2[[1]][,,1]
stk[[1]][,,1]

mean(stk, target="new")

mn.stk.img <- mean.stack.img(stk)
plot(mn.stk.img[,,1])

plot(mn.stk.img[nrow(mn.stk.img[,,1]):1, ,1])


