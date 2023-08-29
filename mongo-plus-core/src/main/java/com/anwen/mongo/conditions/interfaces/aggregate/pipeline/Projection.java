package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

import com.anwen.mongo.support.SFunction;
/**
 * @author JiaChaoYang
 **/
public class Projection {

    private String column;

    private Integer value;

    public static <T> Projection chain(SFunction<T, Object> column, Boolean value) {
        return builder().column(column.getFieldNameLine()).value(value ? 1 : 0).build();
    }

    public static <T> Projection chain(SFunction<T, Object> column, Integer value) {
        return builder().column(column.getFieldNameLine()).value(value).build();
    }

    public static ProjectionBuilder builder() {
        return new ProjectionBuilder();
    }

    public String getColumn() {
        return this.column;
    }

    public Integer getValue() {
        return this.value;
    }

    public void setColumn(String column) {
        this.column = column;
    }

    public void setValue(Integer value) {
        this.value = value;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof Projection)) {
            return false;
        } else {
            Projection other = (Projection)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                Object this$value = this.getValue();
                Object other$value = other.getValue();
                if (this$value == null) {
                    if (other$value != null) {
                        return false;
                    }
                } else if (!this$value.equals(other$value)) {
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
        return other instanceof Projection;
    }

    public int hashCode() {
        int result = 1;
        Object $value = this.getValue();
        result = result * 59 + ($value == null ? 43 : $value.hashCode());
        Object $column = this.getColumn();
        result = result * 59 + ($column == null ? 43 : $column.hashCode());
        return result;
    }

    public String toString() {
        return "Projection(column=" + this.getColumn() + ", value=" + this.getValue() + ")";
    }

    public Projection(String column, Integer value) {
        this.column = column;
        this.value = value;
    }

    public Projection() {
    }

    public static class ProjectionBuilder {
        private String column;
        private Integer value;

        ProjectionBuilder() {
        }

        public ProjectionBuilder column(String column) {
            this.column = column;
            return this;
        }

        public ProjectionBuilder value(Integer value) {
            this.value = value;
            return this;
        }

        public Projection build() {
            return new Projection(this.column, this.value);
        }

        public String toString() {
            return "Projection.ProjectionBuilder(column=" + this.column + ", value=" + this.value + ")";
        }
    }

}
