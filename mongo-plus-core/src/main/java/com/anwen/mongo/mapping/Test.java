package com.anwen.mongo.mapping;

import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.enums.IdTypeEnum;
import com.anwen.mongo.manager.MongoPlusClient;
import com.anwen.mongo.model.BaseModelID;
import org.bson.Document;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class Test implements Serializable {

    private static final long serialVersionUID = 1L;

    @CollectionField("list")
    private ArrayList<LinkedList<Map<String,ConcurrentHashMap<String,Integer>>>> list;

    @CollectionField("baseModelID")
    private BaseModelID baseModelID;

    @CollectionField("idTypeEnum")
    private IdTypeEnum idTypeEnum;

    public ArrayList<LinkedList<Map<String,ConcurrentHashMap<String,Integer>>>> getList() {
        return list;
    }

    public void setList(ArrayList<LinkedList<Map<String,ConcurrentHashMap<String,Integer>>>> list) {
        this.list = list;
    }

    public BaseModelID getBaseModelID() {

        return baseModelID;
    }

    public void setBaseModelID(BaseModelID baseModelID) {
        this.baseModelID = baseModelID;
    }

    public IdTypeEnum getIdTypeEnum() {
        return idTypeEnum;
    }

    public void setIdTypeEnum(IdTypeEnum idTypeEnum) {
        this.idTypeEnum = idTypeEnum;
    }

    @Override
    public String toString() {
        return "Test{" +
                "list=" + list +
                ", baseModelID=" + baseModelID +
                ", idTypeEnum=" + idTypeEnum +
                '}';
    }


    public static void main(String[] args) {
        Document document = new Document();
        document.put("list",new ArrayList<Document>(){{
            add(new Document("a",new Document("a",1)));
            add(new Document("b",new Document("b",2)));
            add(new Document("c",new Document("c",3)));
        }});
        document.put("baseModelID",new Document("_id",1));
        document.put("idTypeEnum","ASSIGN_UUID");
        MongoConverter mongoConverter = new MappingMongoConverter(new MongoPlusClient());
        /*TypeInformation typeInformation = TypeInformation.of(Test.class);
        FieldInformation annotationField = typeInformation.getAnnotationField(ID.class, "");
        Object read = mappingMongoConverter.read(annotationField, document.get("list"), annotationField.getTypeClass());
        annotationField.setValue(read);*/
        Test instance = mongoConverter.read(document, Test.class);
//        Test instance = typeInformation.getInstance();
        ArrayList<LinkedList<Map<String, ConcurrentHashMap<String, Integer>>>> list1 = instance.getList();
        LinkedList<Map<String, ConcurrentHashMap<String, Integer>>> maps = list1.get(0);
        maps.forEach(stringConcurrentHashMapMap -> stringConcurrentHashMapMap.forEach((k, v) -> System.out.println(k+"--"+v)));
        System.out.println(instance);
    }

}
