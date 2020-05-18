library(orderbook)
filename <- system.file("extdata",
"sample.txt",
package = "orderbook")
ob <- orderbook(file = filename)
ob <- read.orders(ob, 10000)
ob
# An object of class orderbook (default)
# --------------------------
# Current orderbook time: 09:35:02
# Message Index: 10,000
# Bid Orders: 631
# Ask Orders: 1,856
# Total Orders: 2,487
summary(ob)
# Current time is 09:35:02
# Ask price levels: 540
# Bid price levels: 179
# Total price levels: 719
# -----------------------------
# Ask orders: 1,856
# Bid orders: 631
# Total orders: 2,487
# -----------------------------
# Spread: 0.02
# Mid point: 11.37
# -----------------------------
# Inside market
# Best Bid: 11.36
# Size: 2,700
# Best Ask: 11.38
# Size: 400
display(ob)
# Current time is 09:35:02
# Price Ask Size
# ---------------------------------------------
# 11.42 900
# 11.41 1,400
# 11.40 1,205
# 11.39 1,600
# 11.38 400
# ---------------------------------------------
# 2,700 11.36
# 1,100 11.35
# 1,100 11.34
# 1,600 11.33
# 700 11.32
# ---------------------------------------------
# Bid Size Price
plot(ob)
# A,34226788,5933949,25.91,100,BID,FALSE
ob["11.01"]
# price size type time id
# 1 11.01 109 BID 34220988 4403084
# 2 11.01 50000 BID 34220988 4403085
# 3 11.01 100 BID 34220988 4403086
nrow(ob["11.00"])
# [1] 56
plot(ob, bounds = 0.033, type = 'o')
plot(ob, bounds = 0.01, type = "sd")
ob <- read.time(ob, "9:30:00")
ob <- read.orders(ob, n = -50)
ob
# An object of class orderbook (default)
# --------------------------
# Current orderbook time: 09:28:41
# Message Index: 292
# Bid Orders: 72
# Ask Orders: 81
# Total Orders: 153
filename <- system.file("extdata",
"tradersample.txt",
package = "orderbook")
ob <- orderbook(file = filename)
ob <- read.time(ob, "9:30:05")
ob <- next.trade(ob)
ob
# An object of class orderbook (trader)
# --------------------------
# Current orderbook time: 09:30:05
# Message Index: 6,062
# Bid Orders: 164
# Ask Orders: 252
# Total Orders: 416
view.trade(ob, tradenum = 584)
# trade 584
# row 6063
# time 09:30:05
# id 636783
# price 25.94
# size 1000
# status FALSE
mid.point(ob)
# price
midpoint.return(ob, tradenum = 584, time = 10)
# midpoint.return
# 10 second 0.065
ob <- read.time(ob, "9:30:15")
plot(ob, type = "t", bounds = 0.02)
ob <- add.order(ob, 11.20, 300, "ASK")
ob <- remove.order(ob, 1231883)
ob <- replace.order(ob, 1231883, 150)
ob <- market.order(ob, 200, "BUY")
ob <- simulate(ob)
plot(ob)
# dave@kanecap.com
# Andrew.T.Liu@williams.edu
# knguyen@cs.umb.edu
# Size (Shares)
# Price
# 10.20
# 10.40
# 10.60
# 10.80
# 11.00
# 11.20
# 11.36
# 0
# 11.38
# 11.60
# 11.80
# 12.00
# 12.20
# 12.40
# 12.60</div>
# Number of Orders
# Price
# 11.00
# 11.10
# 11.20
# 11.30
# 11.36
# 11.38
# 11.50
# 11.60
# 11.70</div>
# Size (%)
# Price (%)
# −150
# −100
# −50
# 0
# 50
# 100
# 150
# 0 20 40 60 80 100 120</div>
# Order Book − 09:35:02
