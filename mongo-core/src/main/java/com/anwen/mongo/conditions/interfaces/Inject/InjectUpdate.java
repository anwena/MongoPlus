package com.anwen.mongo.conditions.interfaces.Inject;

public interface InjectUpdate<Children> {
    
    Children set(String column,Object value);

    Children set(boolean condition,String column,Object value);
    
}
