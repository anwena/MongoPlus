package com.anwen.mongo.conditions.interfaces.condition;

import com.anwen.mongo.enums.CompareEnum;
import com.anwen.mongo.enums.LogicTypeEnum;
import com.anwen.mongo.enums.QueryOperatorEnum;
import com.anwen.mongo.support.SFunction;

import java.util.List;

/**
 * 构建条件对象
 * @author JiaChaoYang
 * @since 2023/2/14 14:13
*/
public class CompareCondition {

    /**
     * 条件
     * @since 2023/2/10 10:16
     */
    private String condition;

    /**
     * 字段
     * @since 2023/2/10 10:16
    */
    private String column;

    /**
     * 值
     * @since 2023/2/10 10:16
    */
    private Object value;

    /**
     * 类型 0查询，1修改
     * @author JiaChaoYang
     * @date 2023/6/25/025 1:49
    */
    private Integer type;

    /**
     * 逻辑运算符类型 0 and 1 or
     * @author JiaChaoYang
     * @date 2023/7/16 19:07
    */
    private Integer logicType;

    /**
     * 子条件
     * @author JiaChaoYang
     * @date 2023/7/16 19:22
    */
    private List<CompareCondition> childCondition;

    public static CompareCondition build(String condition, String column, Object value) {
        return builder().condition(condition).column(column).value(value).type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.AND.getKey()).build();
    }

    public static <T> CompareCondition build(String condition, SFunction<T, Object> column, Object value) {
        return builder().condition(condition).column(column.getFieldNameLine()).value(value).type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.AND.getKey()).build();
    }

    public static <T> CompareCondition build(QueryOperatorEnum condition, SFunction<T, Object> column, Object value) {
        return builder().condition(condition.getValue()).column(column.getFieldNameLine()).value(value).type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.AND.getKey()).build();
    }

    public static CompareCondition buildOr(List<CompareCondition> compareConditionList) {
        return builder().type(CompareEnum.QUERY.getKey()).logicType(LogicTypeEnum.OR.getKey()).childCondition(compareConditionList).build();
    }

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

    public Integer getType() {
        return this.type;
    }

    public Integer getLogicType() {
        return this.logicType;
    }

    public List<CompareCondition> getChildCondition() {
        return this.childCondition;
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

    public void setType(Integer type) {
        this.type = type;
    }

    public void setLogicType(Integer logicType) {
        this.logicType = logicType;
    }

    public void setChildCondition(List<CompareCondition> childCondition) {
        this.childCondition = childCondition;
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
                Object this$type = this.getType();
                Object other$type = other.getType();
                if (this$type == null) {
                    if (other$type != null) {
                        return false;
                    }
                } else if (!this$type.equals(other$type)) {
                    return false;
                }

                Object this$logicType = this.getLogicType();
                Object other$logicType = other.getLogicType();
                if (this$logicType == null) {
                    if (other$logicType != null) {
                        return false;
                    }
                } else if (!this$logicType.equals(other$logicType)) {
                    return false;
                }

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

                Object this$childCondition = this.getChildCondition();
                Object other$childCondition = other.getChildCondition();
                if (this$childCondition == null) {
                    if (other$childCondition != null) {
                        return false;
                    }
                } else if (!this$childCondition.equals(other$childCondition)) {
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
        Object $type = this.getType();
        result = result * 59 + ($type == null ? 43 : $type.hashCode());
        Object $logicType = this.getLogicType();
        result = result * 59 + ($logicType == null ? 43 : $logicType.hashCode());
        Object $condition = this.getCondition();
        result = result * 59 + ($condition == null ? 43 : $condition.hashCode());
        Object $column = this.getColumn();
        result = result * 59 + ($column == null ? 43 : $column.hashCode());
        Object $value = this.getValue();
        result = result * 59 + ($value == null ? 43 : $value.hashCode());
        Object $childCondition = this.getChildCondition();
        result = result * 59 + ($childCondition == null ? 43 : $childCondition.hashCode());
        return result;
    }

    public String toString() {
        return "CompareCondition(condition=" + this.getCondition() + ", column=" + this.getColumn() + ", value=" + this.getValue() + ", type=" + this.getType() + ", logicType=" + this.getLogicType() + ", childCondition=" + this.getChildCondition() + ")";
    }

    public CompareCondition(String condition, String column, Object value, Integer type, Integer logicType, List<CompareCondition> childCondition) {
        this.condition = condition;
        this.column = column;
        this.value = value;
        this.type = type;
        this.logicType = logicType;
        this.childCondition = childCondition;
    }

    public CompareCondition() {
    }

    public static class CompareConditionBuilder {
        private String condition;
        private String column;
        private Object value;
        private Integer type;
        private Integer logicType;
        private List<CompareCondition> childCondition;

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

        public CompareConditionBuilder type(Integer type) {
            this.type = type;
            return this;
        }

        public CompareConditionBuilder logicType(Integer logicType) {
            this.logicType = logicType;
            return this;
        }

        public CompareConditionBuilder childCondition(List<CompareCondition> childCondition) {
            this.childCondition = childCondition;
            return this;
        }

        public CompareCondition build() {
            return new CompareCondition(this.condition, this.column, this.value, this.type, this.logicType, this.childCondition);
        }

        public String toString() {
            return "CompareCondition.CompareConditionBuilder(condition=" + this.condition + ", column=" + this.column + ", value=" + this.value + ", type=" + this.type + ", logicType=" + this.logicType + ", childCondition=" + this.childCondition + ")";
        }
    }

}
