package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

/**
 * replaceRoot类
 *
 * @author JiaChaoYang
 **/
public class ReplaceRoot {

    /**
     * 结果映射字段
     * @author JiaChaoYang
     * @date 2023/8/20 1:54
    */
    private String resultMappingField;

    /**
     * 指定字段
     * @author JiaChaoYang
     * @date 2023/8/20 1:54
    */
    private String field;

    public String getResultMappingField() {
        return this.resultMappingField;
    }

    public String getField() {
        return this.field;
    }

    public void setResultMappingField(String resultMappingField) {
        this.resultMappingField = resultMappingField;
    }

    public void setField(String field) {
        this.field = field;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof ReplaceRoot)) {
            return false;
        } else {
            ReplaceRoot other = (ReplaceRoot)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                Object this$resultMappingField = this.getResultMappingField();
                Object other$resultMappingField = other.getResultMappingField();
                if (this$resultMappingField == null) {
                    if (other$resultMappingField != null) {
                        return false;
                    }
                } else if (!this$resultMappingField.equals(other$resultMappingField)) {
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
        return other instanceof ReplaceRoot;
    }

    public int hashCode() {
        int result = 1;
        Object $resultMappingField = this.getResultMappingField();
        result = result * 59 + ($resultMappingField == null ? 43 : $resultMappingField.hashCode());
        Object $field = this.getField();
        result = result * 59 + ($field == null ? 43 : $field.hashCode());
        return result;
    }

    public String toString() {
        return "ReplaceRoot(resultMappingField=" + this.getResultMappingField() + ", field=" + this.getField() + ")";
    }

    public ReplaceRoot(String resultMappingField, String field) {
        this.resultMappingField = resultMappingField;
        this.field = field;
    }

    public ReplaceRoot() {
    }

}
