package com.anwen.mongo.conditions.interfaces.condition;

import java.lang.reflect.Field;
import java.util.List;
import java.util.Objects;

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

    /**
     * 原始class
     * @date 2024/6/30 下午3:42
     */
    private Class<?> originalClass;

    /**
     * 原始Field
     * @date 2024/6/30 下午4:14
     */
    private Field originalField;

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

    public Class<?> getOriginalClass() {
        return originalClass;
    }

    public void setOriginalClass(Class<?> originalClass) {
        this.originalClass = originalClass;
    }

    public Field getOriginalField() {
        return originalField;
    }

    public void setOriginalField(Field originalField) {
        this.originalField = originalField;
    }

    @Override
    public boolean equals(Object object) {
        if (this == object) return true;
        if (object == null || getClass() != object.getClass()) return false;
        CompareCondition that = (CompareCondition) object;
        return Objects.equals(condition, that.condition) && Objects.equals(column, that.column) && Objects.equals(value, that.value) && Objects.equals(originalClass, that.originalClass) && Objects.equals(originalField, that.originalField);
    }

    @Override
    public int hashCode() {
        return Objects.hash(condition, column, value, originalClass, originalField);
    }

    @Override
    public String toString() {
        return "{" +
                "condition='" + condition + '\'' +
                "column='" + column + '\'' +
                "value=" + value +
                "originalClass=" + originalClass +
                "originalField=" + originalField +
                '}';
    }

    public CompareCondition(String condition, String column, Object value, Class<?> originalClass, Field originalField) {
        this.condition = condition;
        this.column = column;
        this.value = value;
        this.originalClass = originalClass;
        this.originalField = originalField;
    }

    public CompareCondition(String condition, String value,Class<?> originalClass, Field originalField){
        this.condition = condition;
        this.value = value;
        this.originalClass = originalClass;
        this.originalField = originalField;
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
        private Class<?> originalClass;
        private Field originalField;
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

        public CompareConditionBuilder originalClass(Class<?> originalClass) {
            this.originalClass = originalClass;
            return this;
        }

        public CompareConditionBuilder originalField(Field originalField) {
            this.originalField = originalField;
            return this;
        }

        public CompareCondition build() {
            return new CompareCondition(this.condition, this.column, this.value,this.originalClass, this.originalField);
        }

        @Override
        public String toString() {
            return "{" +
                    "condition='" + condition + '\'' +
                    "column='" + column + '\'' +
                    "value=" + value +
                    "originalClass=" + originalClass +
                    "originalField=" + originalField +
                    '}';
        }
    }

}
