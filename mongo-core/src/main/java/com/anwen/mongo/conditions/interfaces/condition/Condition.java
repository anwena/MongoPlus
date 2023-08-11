package com.anwen.mongo.conditions.interfaces.condition;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class Condition {

    private List<CompareCondition> compareConditionList;

    private List<Order> orderList;

}
