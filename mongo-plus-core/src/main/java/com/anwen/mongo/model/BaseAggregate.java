package com.anwen.mongo.model;

import com.anwen.mongo.strategy.aggregate.PipelineStrategy;

import java.util.Objects;

/**
 * @author JiaChaoYang
 **/
public class BaseAggregate {

    private String type;

    private Integer order;

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

    public Integer getOrder() {
        return order;
    }

    public void setOrder(Integer order) {
        this.order = order;
    }

    public BaseAggregate(String type, PipelineStrategy pipelineStrategy, Integer order) {
        this.type = type;
        this.order = order;
        this.pipelineStrategy = pipelineStrategy;
    }

    public BaseAggregate() {
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (!(o instanceof BaseAggregate)) return false;
        BaseAggregate that = (BaseAggregate) o;
        return Objects.equals(getType(), that.getType()) && Objects.equals(getOrder(), that.getOrder()) && Objects.equals(getPipelineStrategy(), that.getPipelineStrategy());
    }

    @Override
    public int hashCode() {
        return Objects.hash(getType(), getOrder(), getPipelineStrategy());
    }

    @Override
    public String toString() {
        return "BaseAggregate{" +
                "type='" + type + '\'' +
                ", order=" + order +
                ", pipelineStrategy=" + pipelineStrategy +
                '}';
    }
}
