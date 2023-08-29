package com.anwen.mongo.model;

import com.anwen.mongo.strategy.aggregate.PipelineStrategy;

/**
 * @author JiaChaoYang
 **/
public class BaseAggregate {

    private String type;

    private PipelineStrategy pipelineStrategy;

    public String getType() {
        return this.type;
    }

    public PipelineStrategy getPipelineStrategy() {
        return this.pipelineStrategy;
    }

    public void setType(String type) {
        this.type = type;
    }

    public void setPipelineStrategy(PipelineStrategy pipelineStrategy) {
        this.pipelineStrategy = pipelineStrategy;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof BaseAggregate)) {
            return false;
        } else {
            BaseAggregate other = (BaseAggregate)o;
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

                Object this$pipelineStrategy = this.getPipelineStrategy();
                Object other$pipelineStrategy = other.getPipelineStrategy();
                if (this$pipelineStrategy == null) {
                    if (other$pipelineStrategy != null) {
                        return false;
                    }
                } else if (!this$pipelineStrategy.equals(other$pipelineStrategy)) {
                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof BaseAggregate;
    }

    public int hashCode() {
        int result = 1;
        Object $type = this.getType();
        result = result * 59 + ($type == null ? 43 : $type.hashCode());
        Object $pipelineStrategy = this.getPipelineStrategy();
        result = result * 59 + ($pipelineStrategy == null ? 43 : $pipelineStrategy.hashCode());
        return result;
    }

    public String toString() {
        return "BaseAggregate(type=" + this.getType() + ", pipelineStrategy=" + this.getPipelineStrategy() + ")";
    }

    public BaseAggregate(String type, PipelineStrategy pipelineStrategy) {
        this.type = type;
        this.pipelineStrategy = pipelineStrategy;
    }

    public BaseAggregate() {
    }

}
