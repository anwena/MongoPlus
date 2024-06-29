package com.anwen.mongo.aware.impl;

import com.anwen.mongo.annotation.aware.AwareInvoke;
import com.anwen.mongo.aware.Aware;

public interface NamespaceAware extends Aware {

    @AwareInvoke
    void nameSpaceAware(Namespace namespace);

    class NamespaceBuild {

        private final Namespace namespace = new Namespace();

        private NamespaceBuild() {
        }

        public static NamespaceBuild builder() {
            return new NamespaceBuild();
        }

        public NamespaceBuild dataBase(String dataBase) {
            namespace.setDataBase(dataBase);
            return this;
        }

        public NamespaceBuild collectionName(String collectionName) {
            namespace.setCollectionName(collectionName);
            return this;
        }

        public NamespaceBuild entityClass(Class<?> entityClass) {
            namespace.setEntityClass(entityClass);
            return this;
        }

        public Namespace build() {
            return this.namespace;
        }

    }

    class Namespace {

        private String dataBase;

        private String collectionName;

        private Class<?> entityClass;

        private Namespace() {
        }

        public String getDataBase() {
            return dataBase;
        }

        public void setDataBase(String dataBase) {
            this.dataBase = dataBase;
        }

        public String getCollectionName() {
            return collectionName;
        }

        public void setCollectionName(String collectionName) {
            this.collectionName = collectionName;
        }

        public Class<?> getEntityClass() {
            return entityClass;
        }

        public void setEntityClass(Class<?> entityClass) {
            this.entityClass = entityClass;
        }
    }

}
