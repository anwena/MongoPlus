package com.anwen.mongo.toolkit;

import org.bson.Document;

import java.util.concurrent.atomic.AtomicReference;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-11-24 13:36
 **/
public class DocumentUpdater {

    private final AtomicReference<Document> docRef;

    public DocumentUpdater(Document doc) {
        this.docRef = new AtomicReference<>(doc);
    }

    public Document update(String key, Object value) {
        Document oldDoc, newDoc;
        do {
            oldDoc = docRef.get();
            newDoc = new Document(oldDoc);
            newDoc.put(key, value);
        } while (!docRef.compareAndSet(oldDoc, newDoc));
        return newDoc;
    }

}
