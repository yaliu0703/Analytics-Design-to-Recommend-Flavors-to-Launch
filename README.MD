# Analytics Design to Recommend Flavors to Launch

Ya Liu

ya.liu1@simon.rochester.edu

https://github.com/yaliu0703

https://www.linkedin.com/in/yaliu1/

In this project, I am conducting analytics to provide recommendations to the decision of which flavors to launch next. Assume for this analysis that the private label recently launched six flavors--Blueberry, Honey, Peach, Plain, Strawberry, and Vanilla. I focused on providing analytics to help determine what the next flavors to launch should be.

# 1. Extract data


Use MySQL to extract data from datasets.

```
show databases;
use retailer1;
show tables;
```
Firstly, let's describe the percentage of sales of existing flavors in the Greek yogurt category (all brands).

```
select Flavor1,sum(Sales)
from storeItemSales,itemsAttributes
where storeItemSales.`Item.Num`=itemsAttributes.`Item.Num` AND Class="GREEK" 
group by Flavor1
order by sum(Sales) desc;
```
Now, let's look at the percentage of sales of existing flavors in the regular yogurt category (all brands). 

```
select Flavor1,sum(Sales)
from storeItemSales,itemsAttributes
where storeItemSales.`Item.Num`=itemsAttributes.`Item.Num` AND Class="REGULAR" 
group by Flavor1
order by sum(Sales) desc;
Describe the percentage of sales of existing flavors in the regular yogurt category (all brands) 
select Flavor1,sum(Sales)
from storeItemSales,itemsAttributes
where storeItemSales.`Item.Num`=itemsAttributes.`Item.Num` AND Class="REGULAR" 
group by Flavor1
order by sum(Sales) desc;
```



