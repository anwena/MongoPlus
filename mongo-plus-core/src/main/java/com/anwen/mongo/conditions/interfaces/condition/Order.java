package com.anwen.mongo.conditions.interfaces.condition;

/**
 * @author JiaChaoYang
 * 排序配置
 * @since 2023-02-10 11:57
 **/
public class Order {

    /**
     * 排序类型 1升序  -1降序
     * @since 2023/2/10 11:59
    */
    private Integer type;

    /**
     * 字段名
     * @since 2023/2/10 11:59
    */
    private String column;

    public Integer getType() {
        return this.type;
    }

    public String getColumn() {
        return this.column;
    }

    public void setType(Integer type) {
        this.type = type;
    }

    public void setColumn(String column) {
        this.column = column;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof Order)) {
            return false;
        } else {
            Order other = (Order)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                Object this$type = this.getType();
                Object other$type = other.getType();
                if (this$type == null) {
                    if (other$type != null) {
                        return false;
                    }
                } else if (!this$type.equals(other$type)) {
                    return false;
                }

                Object this$column = this.getColumn();
                Object other$column = other.getColumn();
                if (this$column == null) {
                    if (other$column != null) {
                        return false;
                    }
                } else if (!this$column.equals(other$column)) {
                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof Order;
    }

    public int hashCode() {
        int result = 1;
        Object $type = this.getType();
        result = result * 59 + ($type == null ? 43 : $type.hashCode());
        Object $column = this.getColumn();
        result = result * 59 + ($column == null ? 43 : $column.hashCode());
        return result;
    }

    public String toString() {
        return "Order(type=" + this.getType() + ", column=" + this.getColumn() + ")";
    }

    public Order(Integer type, String column) {
        this.type = type;
        this.column = column;
    }

    public Order() {
    }

}
