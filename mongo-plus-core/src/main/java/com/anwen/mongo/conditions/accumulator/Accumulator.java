package com.anwen.mongo.conditions.accumulator;

/**
 * 累加器结果集
 *
 * @author JiaChaoYang
 **/
public class Accumulator {

    /**
     * 结果映射字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:22
    */
    private String resultMappingField;

    /**
     * 条件
     * @author JiaChaoYang
     * @date 2023/8/17 20:11
    */
    private String condition;

    /**
     * 字段
     * @author JiaChaoYang
     * @date 2023/8/17 20:11
    */
    private String field;

    public Accumulator(String condition, String field) {
        this.condition = condition;
        this.field = field;
        this.resultMappingField = field;
    }

    public static AccumulatorBuilder builder() {
        return new AccumulatorBuilder();
    }

    public String getResultMappingField() {
        return this.resultMappingField;
    }

    public String getCondition() {
        return this.condition;
    }

    public String getField() {
        return this.field;
    }

    public void setResultMappingField(String resultMappingField) {
        this.resultMappingField = resultMappingField;
    }

    public void setCondition(String condition) {
        this.condition = condition;
    }

    public void setField(String field) {
        this.field = field;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof Accumulator)) {
            return false;
        } else {
            Accumulator other = (Accumulator)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                label47: {
                    Object this$resultMappingField = this.getResultMappingField();
                    Object other$resultMappingField = other.getResultMappingField();
                    if (this$resultMappingField == null) {
                        if (other$resultMappingField == null) {
                            break label47;
                        }
                    } else if (this$resultMappingField.equals(other$resultMappingField)) {
                        break label47;
                    }

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

                Object this$field = this.getField();
                Object other$field = other.getField();
                if (this$field == null) {
                    if (other$field != null) {
                        return false;
                    }
                } else if (!this$field.equals(other$field)) {
                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof Accumulator;
    }

    public int hashCode() {
        int result = 1;
        Object $resultMappingField = this.getResultMappingField();
        result = result * 59 + ($resultMappingField == null ? 43 : $resultMappingField.hashCode());
        Object $condition = this.getCondition();
        result = result * 59 + ($condition == null ? 43 : $condition.hashCode());
        Object $field = this.getField();
        result = result * 59 + ($field == null ? 43 : $field.hashCode());
        return result;
    }

    public String toString() {
        return "Accumulator(resultMappingField=" + this.getResultMappingField() + ", condition=" + this.getCondition() + ", field=" + this.getField() + ")";
    }

    public Accumulator(String resultMappingField, String condition, String field) {
        this.resultMappingField = resultMappingField;
        this.condition = condition;
        this.field = field;
    }

    public Accumulator() {
    }

    public static class AccumulatorBuilder {
        private String resultMappingField;
        private String condition;
        private String field;

        AccumulatorBuilder() {
        }

        public AccumulatorBuilder resultMappingField(String resultMappingField) {
            this.resultMappingField = resultMappingField;
            return this;
        }

        public AccumulatorBuilder condition(String condition) {
            this.condition = condition;
            return this;
        }

        public AccumulatorBuilder field(String field) {
            this.field = field;
            return this;
        }

        public Accumulator build() {
            return new Accumulator(this.resultMappingField, this.condition, this.field);
        }

        public String toString() {
            return "Accumulator.AccumulatorBuilder(resultMappingField=" + this.resultMappingField + ", condition=" + this.condition + ", field=" + this.field + ")";
        }
    }
}
