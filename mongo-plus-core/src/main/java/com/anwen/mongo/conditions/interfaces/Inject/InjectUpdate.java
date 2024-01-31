package com.anwen.mongo.conditions.interfaces.Inject;

import java.util.List;

public interface InjectUpdate<Children> {
    
    Children set(String column,Object value);

    Children set(boolean condition,String column,Object value);

    Children push(String column,Object value);

    Children push(boolean condition,String column,Object value);

    Children push(String column,Object ... value);

    Children push(boolean condition,String column,Object ... value);

    Children push(String column, List<?> value);

    Children push(boolean condition,String column,List<?> value);

}
