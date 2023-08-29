package com.anwen.mongo.model;

import java.util.List;

/**
 * @author JiaChaoYang
 * @project mongo-plus
 * @description
 * @date 2023-02-23 10:57
 **/
public class PageResult<T> {
    /**
     * 当前页
     * @author JiaChaoYang
     * @date 2023/2/23 10:58
     */
    private long pageNum;

    /**
     * 每页显示行数
     * @author JiaChaoYang
     * @date 2023/2/23 10:58
    */
    private long pageSize;

    /**
     * 总行数
     * @author JiaChaoYang
     * @date 2023/2/23 10:58
    */
    private long totalSize;

    /**
     * 页码总数
     * @author JiaChaoYang
     * @date 2023/2/23 10:58
    */
    private long totalPages;

    /**
     * 数据模型
     * @author JiaChaoYang
     * @date 2023/2/23 10:59
    */
    private List<T> contentData;

    public long getPageNum() {
        return this.pageNum;
    }

    public long getPageSize() {
        return this.pageSize;
    }

    public long getTotalSize() {
        return this.totalSize;
    }

    public long getTotalPages() {
        return this.totalPages;
    }

    public List<T> getContentData() {
        return this.contentData;
    }

    public void setPageNum(long pageNum) {
        this.pageNum = pageNum;
    }

    public void setPageSize(long pageSize) {
        this.pageSize = pageSize;
    }

    public void setTotalSize(long totalSize) {
        this.totalSize = totalSize;
    }

    public void setTotalPages(long totalPages) {
        this.totalPages = totalPages;
    }

    public void setContentData(List<T> contentData) {
        this.contentData = contentData;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        } else if (!(o instanceof PageResult)) {
            return false;
        } else {
            PageResult<?> other = (PageResult)o;
            if (!other.canEqual(this)) {
                return false;
            } else if (this.getPageNum() != other.getPageNum()) {
                return false;
            } else if (this.getPageSize() != other.getPageSize()) {
                return false;
            } else if (this.getTotalSize() != other.getTotalSize()) {
                return false;
            } else if (this.getTotalPages() != other.getTotalPages()) {
                return false;
            } else {
                Object this$contentData = this.getContentData();
                Object other$contentData = other.getContentData();
                if (this$contentData == null) {
                    if (other$contentData != null) {
                        return false;
                    }
                } else if (!this$contentData.equals(other$contentData)) {
                    return false;
                }

                return true;
            }
        }
    }

    protected boolean canEqual(Object other) {
        return other instanceof PageResult;
    }

    public int hashCode() {
        int result = 1;
        long $pageNum = this.getPageNum();
        result = result * 59 + (int)($pageNum >>> 32 ^ $pageNum);
        long $pageSize = this.getPageSize();
        result = result * 59 + (int)($pageSize >>> 32 ^ $pageSize);
        long $totalSize = this.getTotalSize();
        result = result * 59 + (int)($totalSize >>> 32 ^ $totalSize);
        long $totalPages = this.getTotalPages();
        result = result * 59 + (int)($totalPages >>> 32 ^ $totalPages);
        Object $contentData = this.getContentData();
        result = result * 59 + ($contentData == null ? 43 : $contentData.hashCode());
        return result;
    }

    public String toString() {
        return "PageResult(pageNum=" + this.getPageNum() + ", pageSize=" + this.getPageSize() + ", totalSize=" + this.getTotalSize() + ", totalPages=" + this.getTotalPages() + ", contentData=" + this.getContentData() + ")";
    }

    public PageResult(long pageNum, long pageSize, long totalSize, long totalPages, List<T> contentData) {
        this.pageNum = pageNum;
        this.pageSize = pageSize;
        this.totalSize = totalSize;
        this.totalPages = totalPages;
        this.contentData = contentData;
    }

    public PageResult() {
    }
}
