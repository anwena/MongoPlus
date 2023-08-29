package com.anwen.mongo.conditions.interfaces.aggregate.pipeline;

/**
 * let操作符
 *
 * @author JiaChaoYang
 **/
public class Let {

    /**
     * 变量
     * @author JiaChaoYang
     * @date 2023/8/21 22:09
    */
    private String variable;

    /**
     * 值
     * @author JiaChaoYang
     * @date 2023/8/21 22:09
    */
    private String value;

    public static LetBuilder builder() {
        return new LetBuilder();
    }

    public String getVariable() {
        return this.variable;
    }

    public String getValue() {
        return this.value;
    }

    public void setVariable(String variable) {
        this.variable = variable;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof Let)) {
            return false;
        } else {
            Let other = (Let)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                Object this$variable = this.getVariable();
                Object other$variable = other.getVariable();
                if (this$variable == null) {
                    if (other$variable != null) {
                        return false;
                    }
                } else if (!this$variable.equals(other$variable)) {
                    return false;
                }

                Object this$value = this.getValue();
                Object other$value = other.getValue();
                if (this$value == null) {
                    if (other$value != null) {
                        return false;
                    }
                } else if (!this$value.equals(other$value)) {
                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof Let;
    }

    public int hashCode() {
        int result = 1;
        Object $variable = this.getVariable();
        result = result * 59 + ($variable == null ? 43 : $variable.hashCode());
        Object $value = this.getValue();
        result = result * 59 + ($value == null ? 43 : $value.hashCode());
        return result;
    }

    public String toString() {
        return "Let(variable=" + this.getVariable() + ", value=" + this.getValue() + ")";
    }

    public Let(String variable, String value) {
        this.variable = variable;
        this.value = value;
    }

    public Let() {
    }

    public static class LetBuilder {
        private String variable;
        private String value;

        LetBuilder() {
        }

        public LetBuilder variable(String variable) {
            this.variable = variable;
            return this;
        }

        public LetBuilder value(String value) {
            this.value = value;
            return this;
        }

        public Let build() {
            return new Let(this.variable, this.value);
        }

        public String toString() {
            return "Let.LetBuilder(variable=" + this.variable + ", value=" + this.value + ")";
        }
    }

}
