package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.ID;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class Test {

    public ArrayList<LinkedList<Map<String,ConcurrentHashMap<String,Integer>>>> getList() {
        return list;
    }

    public void setList(ArrayList<LinkedList<Map<String,ConcurrentHashMap<String,Integer>>>> list) {
        this.list = list;
    }

    @ID
    private ArrayList<LinkedList<Map<String,ConcurrentHashMap<String,Integer>>>> list;

}
