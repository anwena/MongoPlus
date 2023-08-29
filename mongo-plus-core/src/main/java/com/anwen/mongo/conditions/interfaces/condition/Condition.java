package com.anwen.mongo.conditions.interfaces.condition;

import java.util.List;

public class Condition {

    private List<CompareCondition> compareConditionList;

    private List<Order> orderList;

    public List<CompareCondition> getCompareConditionList() {
        return this.compareConditionList;
    }

    public List<Order> getOrderList() {
        return this.orderList;
    }

    public void setCompareConditionList(List<CompareCondition> compareConditionList) {
        this.compareConditionList = compareConditionList;
    }

    public void setOrderList(List<Order> orderList) {
        this.orderList = orderList;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof Condition)) {
            return false;
        } else {
            Condition other = (Condition)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                Object this$compareConditionList = this.getCompareConditionList();
                Object other$compareConditionList = other.getCompareConditionList();
                if (this$compareConditionList == null) {
                    if (other$compareConditionList != null) {
                        return false;
                    }
                } else if (!this$compareConditionList.equals(other$compareConditionList)) {
                    return false;
                }

                Object this$orderList = this.getOrderList();
                Object other$orderList = other.getOrderList();
                if (this$orderList == null) {
                    if (other$orderList != null) {
                        return false;
                    }
                } else if (!this$orderList.equals(other$orderList)) {
                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof Condition;
    }

    public int hashCode() {
        int result = 1;
        Object $compareConditionList = this.getCompareConditionList();
        result = result * 59 + ($compareConditionList == null ? 43 : $compareConditionList.hashCode());
        Object $orderList = this.getOrderList();
        result = result * 59 + ($orderList == null ? 43 : $orderList.hashCode());
        return result;
    }

    public String toString() {
        return "Condition(compareConditionList=" + this.getCompareConditionList() + ", orderList=" + this.getOrderList() + ")";
    }

    public Condition(List<CompareCondition> compareConditionList, List<Order> orderList) {
        this.compareConditionList = compareConditionList;
        this.orderList = orderList;
    }

    public Condition() {
    }

}
