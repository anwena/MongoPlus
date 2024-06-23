package com.anwen.mongo.conditions.interfaces.condition;

import java.util.List;

/**
 * 构建条件对象
 * @author JiaChaoYang
 * @since 2023/2/14 14:13
*/
public class CompareCondition {

    /**
     * 条件
     * @date 2023/2/10 10:16
     */
    private String condition;

    /**
     * 字段
     * @date 2023/2/10 10:16
    */
    private String column;

    /**
     * 值
     * @since 2023/2/10 10:16
    */
    private Object value;

    public static CompareConditionBuilder builder() {
        return new CompareConditionBuilder();
    }

    public String getCondition() {
        return this.condition;
    }

    public String getColumn() {
        return this.column;
    }

    public Object getValue() {
        return this.value;
    }

    public void setCondition(String condition) {
        this.condition = condition;
    }

    public void setColumn(String column) {
        this.column = column;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof CompareCondition)) {
            return false;
        } else {
            CompareCondition other = (CompareCondition)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                Object this$condition = this.getCondition();
                Object other$condition = other.getCondition();
                if (this$condition == null) {
                    if (other$condition != null) {
                        return false;
                    }
                } else if (!this$condition.equals(other$condition)) {
                    return false;
                }

                label62: {
                    Object this$column = this.getColumn();
                    Object other$column = other.getColumn();
                    if (this$column == null) {
                        if (other$column == null) {
                            break label62;
                        }
                    } else if (this$column.equals(other$column)) {
                        break label62;
                    }

                    return false;
                }

                label55: {
                    Object this$value = this.getValue();
                    Object other$value = other.getValue();
                    if (this$value == null) {
                        if (other$value == null) {
                            break label55;
                        }
                    } else if (this$value.equals(other$value)) {
                        break label55;
                    }

                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof CompareCondition;
    }

    public int hashCode() {
        int result = 1;
        Object $condition = this.getCondition();
        result = result * 59 + ($condition == null ? 43 : $condition.hashCode());
        Object $column = this.getColumn();
        result = result * 59 + ($column == null ? 43 : $column.hashCode());
        Object $value = this.getValue();
        result = result * 59 + ($value == null ? 43 : $value.hashCode());
        return result;
    }

    public String toString() {
        return "CompareCondition(condition=" + this.getCondition() + ", column=" + this.getColumn() + ", value=" + this.getValue()+ ")";
    }

    public CompareCondition(String condition, String column, Object value) {
        this.condition = condition;
        this.column = column;
        this.value = value;
    }

    public CompareCondition(String condition, String value){
        this.condition = condition;
        this.value = value;
    }

    public CompareCondition(String condition, List<CompareCondition> compareConditionList){
        this.condition = condition;
        this.value = compareConditionList;
    }

    public CompareCondition() {
    }

    public static class CompareConditionBuilder {
        private String condition;
        private String column;
        private Object value;
        CompareConditionBuilder() {
        }

        public CompareConditionBuilder condition(String condition) {
            this.condition = condition;
            return this;
        }

        public CompareConditionBuilder column(String column) {
            this.column = column;
            return this;
        }

        public CompareConditionBuilder value(Object value) {
            this.value = value;
            return this;
        }


        public CompareCondition build() {
            return new CompareCondition(this.condition, this.column, this.value);
        }

        public String toString() {
            return "CompareCondition.CompareConditionBuilder(condition=" + this.condition + ", column=" + this.column + ", value=" + this.value+ ")";
        }
    }

}
