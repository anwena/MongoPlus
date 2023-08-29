package com.anwen.mongo.model;

import com.anwen.mongo.annotation.ID;
import com.anwen.mongo.annotation.collection.CollectionField;
import com.anwen.mongo.enums.IdTypeEnum;

/**
 * @author JiaChaoYang
 * 基础对象ID
 * @since 2023-02-13 11:52
 **/
public class BaseModelID {

    /**
     * mongoDB生成的id
     * @since 2023/2/13 11:52
    */
    @ID(type = IdTypeEnum.ASSIGN_ID)
    @CollectionField("_id")
    public String id;

    public BaseModelID() {
    }

    public String getId() {
        return this.id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof BaseModelID)) {
            return false;
        } else {
            BaseModelID other = (BaseModelID)o;
            if (!other.canEqual(this)) {
                return false;
            } else {
                Object this$id = this.getId();
                Object other$id = other.getId();
                if (this$id == null) {
                    if (other$id != null) {
                        return false;
                    }
                } else if (!this$id.equals(other$id)) {
                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof BaseModelID;
    }

    public int hashCode() {
        int result = 1;
        Object $id = this.getId();
        result = result * 59 + ($id == null ? 43 : $id.hashCode());
        return result;
    }

    public String toString() {
        return "BaseModelID(id=" + this.getId() + ")";
    }


}
