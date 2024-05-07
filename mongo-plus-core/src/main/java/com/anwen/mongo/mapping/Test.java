package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;

import java.util.List;

public class Test {

    public List<List<Integer>> getList() {
        return list;
    }

    public void setList(List<List<Integer>> list) {
        this.list = list;
    }

    @ID
    private List<List<Integer>> list;

}
