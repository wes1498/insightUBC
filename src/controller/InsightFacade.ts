import Log from "../Util";
import {
    IInsightFacade,
    InsightCourse,
    InsightDataset,
    InsightDatasetKind,
    InsightError,
    InsightFilter,
    NotFoundError,
    InsightOrderObj,
    InsightTransformations,
} from "./IInsightFacade";
import * as JSZip from "jszip";
import {JSZipObject} from "jszip";
import * as fs from "fs";
import {Decimal} from "decimal.js";
import {fileExists} from "ts-node";
import {isNumber, isString} from "util";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {

    private coursesMap: Map<string, object[]>;

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.coursesMap = new Map<string, object[]>();
    }
    private something() {
        this.pathReader("data/");
    }
    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        return new Promise<string[]>((resolve, reject) => {
            this.something();
            let filter: string = "A";
            if (this.coursesMap.has(id)) {
                filter = "has ID";
            } else if (!id || id.length === 0 || id === "") {
                filter = "Not valid";
            }
            switch (filter) {
                case "Not valid": {
                    return reject(new InsightError("Invalid Id"));
                }
                case "has ID": {
                    return reject(new InsightError("Id is already added"));
                }
                default: {
                    this.coursesMap.set(id, []);
                }
            }
            JSZip.loadAsync(content, {base64: true}).then((unzipped: JSZip) => {
                let dataType: string;
                if (kind === InsightDatasetKind.Courses) {
                    dataType = "courses/";
                } else if (kind === InsightDatasetKind.Rooms) {
                    dataType = "rooms/";
                }
                let filesToLoadPromise: any[] = [];
                if (unzipped.length < 1) {
                    return reject(new InsightError("Invalid Id"));
                } else if (unzipped.files.hasOwnProperty(dataType) || unzipped.length > 0) {
                    unzipped.forEach(((relativePath, fileObject: JSZipObject) => {
                        // if (fileObject.dir) {
                        // return reject(new InsightError("Is a folder "));
                        //  } else {
                        filesToLoadPromise.push(fileObject.async("text").then((data: string) => {
                            this.addCourse(data, id); // save the course info a
                        }).catch((e) => {
                            return reject(new InsightError("Error processing encoded course data " + e));
                        }));
                        // }
                    }));
                } else {
                    return reject(new InsightError("Desired folder for the dataset kind does not exist"));
                }
                Promise.all(filesToLoadPromise).then(() => {
                    let toSaveOnDisk: object[] = this.coursesMap.get(id);
                    if (toSaveOnDisk.length === 0) {
                        this.coursesMap.delete(id);
                        return reject(new InsightError("No sections were added"));
                    } else {
                        fs.writeFile("data/" + id, JSON.stringify(toSaveOnDisk), (e) => {
                            return reject(new InsightError("Error Saving Files"));
                        });

                        let result: string[] = [];
                        this.coursesMap.forEach((value, key, map) => {
                            result.push(key);
                        });
                        return resolve(result);
                    }
                });
            }).catch((e) => {
                return reject(new InsightError("Error decoding contents: Invalid Zip " + e));
            });
        });
    }
    private addCourse(course: string, datasetId: string) {
        try {
            const courseInfo: object[] = JSON.parse(course).result;
            let info: any;
            for (info of courseInfo ) {
                let courseDep: string = info.Subject as string;
                let courseId: string = info.Course as string;
                let courseAvg: number = info.Avg as number;
                let courseInstru: string = info.Professor as string;
                let courseTitle: string = info.Title as string;
                let coursePass: number = info.Pass as number;
                let courseFail: number = info.Fail as number;
                let courseAu: number = info.Audit as number;
                let courseUuid: string = info.id.toString() as string;
                let courseYear: number = Number(info.Year);

                let completeCourse: InsightCourse = {
                    coursesDept: courseDep,
                    coursesId: courseId,
                    coursesAvg: courseAvg,
                    coursesInstructor: courseInstru,
                    coursesTitle: courseTitle,
                    coursesPass: coursePass,
                    coursesFail: courseFail,
                    coursesAudit: courseAu,
                    coursesUuid: courseUuid,
                    coursesYear: courseYear
                };
                for (let value of Object.values(completeCourse)) {
                    if (value === undefined || typeof value === "object" || value instanceof Array) {
                        throw new TypeError("Some of the fields are missing or of non-cast type");
                    }
                }
                this.coursesMap.get(datasetId).push(completeCourse);
            }

        } catch (e) {
            // not in JSON format or some fields of different type/missing-> skip this course
        }
    }
    private pathReader(folderName: string): void { // deprecated
        fs.readdir(folderName, (e , files) => {
            if (e) {
                let error1 =  "Error reading dir";
                return error1;
            } else {
                files.forEach((filesId) => {
                    if (this.coursesMap.has(filesId)) {
                        return;
                    } else {
                        let name = folderName + filesId;
                        fs.readFile(name, "utf-8", (e2, content: string) => {
                            if (e2) {
                                return;
                            }
                            this.coursesMap.set(filesId, JSON.parse(content));
                        });
                    }
                });
            }
        });
    }
    public removeDataset(id: string): Promise<string> {
        return new Promise<string>((resolve, reject) => {
            if ( id === null || !id || id === "") {
                return reject(new InsightError("Invalid ID"));
            }

            // if(!this.courseMap.has(id)) return reject(new NotFoundError("Dataset has not yet been loaded"));

            fs.unlink("data/" + id, (err) => {
                 if (err) {
                    return reject(new NotFoundError("Dataset has not yet been loaded"));
                } else if (this.coursesMap.has(id)) {
                    this.coursesMap.delete(id);
                    return resolve(id);
                }
            });
        });
    }
    public performQuery(query: any): Promise <any[]> {// re-code
        return new Promise<any[]>((resolve, reject) => {
        try {
            let filter: InsightFilter = query.WHERE;
            let options = query.OPTIONS;
            let transformations = query.TRANSFORMATIONS;
            let columns = options.COLUMNS;
            let order = options.ORDER;
            let id: string = columns[0].split("_")[0];
            let dataset;
            let result: any;
            if (this.coursesMap.get(id)) {
                dataset = this.coursesMap.get(id);
            } else {
                throw new Error("Can't find dataset with id: " + id);
            }
           /* if (Object.keys(filter).length > 0) {
                dataset = this.filterDataset(dataset, filter, id);
            }
            if (transformations) {
                dataset = this.transformDataset(dataset, transformations);
            }
            let result = this.trimDatasetByColumns(dataset, columns);
            if (order) {
                result = this.sortResult(result, order, columns);
            }
            return resolve(result);*/
            return resolve(result);
        } catch (err) {
            return reject(new InsightError("DWhack"));
        }
    });
}
   /* private isFilterSatisfied(filter: InsightFilter, data: any, cid: string): boolean {
        if (Object.keys(filter).length > 1) {
            throw Error("Query is malformed");
        } else if (filter.GT) {
            const key = Object.keys(filter.GT)[0];
            const val = filter.GT[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const value = data[col];
            if (cid !== fid) {
                throw new Error("Query is trying to compare two datasets at the same time.");
            }
            if (isNumber(val) && isNumber(value)) {
                return (value > val);
            } else {
                throw new Error("Invalid query: GT value should be a number.");
            }
        } else if (filter.EQ) {
            const key = Object.keys(filter.EQ)[0];
            const val = filter.EQ[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const value = data[col];
            if (cid !== fid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (isNumber(val) && isNumber(value)) {
                return (value === val);
            } else {
                throw new Error("Invalid query: EQ value should be a number.");
            }
        } else if (filter.LT) {
            const key = Object.keys(filter.LT)[0];
            const val = filter.LT[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const value: number = data[col];
            if (cid !== fid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (isNumber(val) && isNumber(value)) {
                return (value < val);
            } else {
                throw new Error("Invalid query: LT value should be a number.");
            }
        } else if (filter.IS) {
            const key = Object.keys(filter.IS)[0];
            const val: string = filter.IS[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const value: string = data[col];
            const len = val.length;
            if (cid !== fid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (isString(val) && isString(value)) {
                if (val === "*") {
                    return true;
                } else if (len === 1) {
                    return (val === value);
                } else if (val[0] === "*" && val[len - 1] === "*") {
                    return value.includes(val.substr(1, len - 2));
                } else if (val[0] === "*") {
                    return value.endsWith(val.slice(1));
                } else if (val[len - 1] === "*") {
                    return value.startsWith(val.substring(0, len - 1));
                } else if (val.includes("*")) {
                    throw new Error("Invalid query: * should not be in middle.");
                } else {
                    return (val === value);
                }
            } else {
                throw new Error("Invalid query: IS value should be a string.");
            }
        } else if (filter.NOT) {
            return (!(this.isFilterSatisfied(filter.NOT, data, cid)));
        } else if (filter.AND) {
            if (filter.AND.length === 0) {
                // Log.trace(filter.AND.length.toString());
                throw new Error("Invalid query: AND should contain at least one condition");
            }
            let res = true;
            for (const r of filter.AND) {
                if (this.isFilterSatisfied(r, data, cid) === false) {
                    res = false;
                }
            }
            return res;
        } else if (filter.OR) {
            if (filter.OR.length === 0) {
                throw new Error("Invalid query: OR should contain at least one condition");
            }
            let res = false;
            for (const r of filter.OR) {
                if (this.isFilterSatisfied(r, data, cid) === true) {
                    res = true;
                }
            }
            return res;
        } else {
            throw new Error("Invalid Query.");
        }
    }
    private sortResultByKey(result: any[], key: string, columns: string[]): any[] {
        if (!columns.includes(key)) {
            throw new Error("Order key needs to be included in columns");
        }
        result.sort((left, right): any => {
            if (left[key] > right[key]) {
                return 1;
            } else if (left[key] < right[key]) {
                return -1;
            }
        });
        return result;
    }
    private sortResultByOrderObj(result: any[], order: InsightOrderObj, columns: string[]): any[] {
        const orderKeys = order.keys;
        const maxIndex = orderKeys.length;
        let currIndex: number;
        function isLarger(left: any, right: any, key: string): number {
            if (!columns.includes(key)) {
                throw new Error("Order key needs to be included in columns");
            }
            if (left[key] > right[key]) {
                return 1;
            } else if (left[key] < right [key]) {
                return -1;
            } else {
                currIndex += 1;
                if (currIndex < maxIndex) {
                    return isLarger(left, right, orderKeys[currIndex]);
                } else {
                    return 0;
                }
            }
        }
        result.sort((left, right) => {
            currIndex = 0;
            if (order.dir === "UP") {
                return isLarger(left, right, orderKeys[currIndex]);
            } else if (order.dir === "DOWN") {
                return isLarger(right, left, orderKeys[currIndex]);
            } else {
                throw new Error("Invalid dir value.");
            }
        });
        return result;
    }
    private sortResult(result: any[], order: any, columns: string[]): any[] {
        let sortedResult;
        if (isString(order)) {
            sortedResult = this.sortResultByKey(result, order as string, columns);
        } else {
            sortedResult = this.sortResultByOrderObj(result, order as InsightOrderObj, columns);
        }
        return sortedResult;
    }*/
   /* private transformDataset(dataset: any[], transformations: InsightTransformations): any[] {
        const transformedDataset = [];
        const groups: Map<string, any[]> = new Map<string, any[]>();
        for (const data of dataset) {
            let groupName: string = "";
            for (const ID_KEY of transformations.GROUP) {
                const key = ID_KEY.split("_")[1];
                const value = data[key] as string;
                if (value === undefined) {
                    throw new Error(ID_KEY + " is not a valid key");
                }
                groupName += value;
            }
            if (!groups.get(groupName)) {
                groups.set(groupName, []);
            }
            groups.get(groupName).push(data);
        }
        for (const group of groups.values()) {
            const entry: {[key: string]: any } = {};
            for (const ID_KEY of transformations.GROUP) {
                const key = ID_KEY.split("_")[1];
                entry[key] = group[0][key];
            }
            const addedApplyKeys: string[] = [];
            for (const applyObj of transformations.APPLY) {
                const applyKey = Object.keys(applyObj)[0];
                if (applyKey.includes("_")) {
                    throw new Error("Apply keys cannot contain '_'");
                }
                if (addedApplyKeys.includes(applyKey)) {
                    throw new Error("Duplicate apply key");
                } else {
                    addedApplyKeys.push(applyKey);
                }
                const token = Object.keys(applyObj[applyKey])[0];
                const ID_KEY = applyObj[applyKey][token];
                const key = ID_KEY.split("_")[1];
                const valueSet = [];
                for (const data of group) {
                    valueSet.push(data[key]);
                }
                let value: number;
                switch (token) {
                    case "MAX":
                        value = valueSet[0];
                        for (const elem of valueSet) {
                            if (!isNumber(elem)) {
                                throw new Error("Max supports only numerical values");
                            } else {
                                if (elem > value) {
                                    value = elem;
                                }
                            }
                        }
                        break;
                    case "MIN":
                        value = valueSet[0];
                        for (const elem of valueSet) {
                            if (!isNumber(elem)) {
                                throw new Error("Min supports only numerical values");
                            } else {
                                if (elem < value) {
                                    value = elem;
                                }
                            }
                        }
                        break;
                    case "SUM":
                        let total = new Decimal(0);
                        for (const elem of valueSet) {
                            total = total.add(new Decimal(elem));
                        }
                        value = Number(total.toFixed(2));
                        break;
                    case "AVG":
                        let sum = new Decimal(0);
                        for (const elem of valueSet) {
                            sum = sum.add(new Decimal(elem));
                        }
                        value = Number((Number(sum) / valueSet.length).toFixed(2));
                        break;
                    case "COUNT":
                        const valueSetUnique = valueSet.filter((elem, pos, self) => {
                            return self.indexOf(elem) === pos;
                        });
                        value = valueSetUnique.length;
                        break;
                }
                entry[applyKey] = value;
            }
            transformedDataset.push(entry);
        }
        return transformedDataset;
    }
    private filterDataset(dataset: any[], filter: InsightFilter, id: string): any[] {
        const filteredDataset: any[] = [];
        for (const data of dataset) {
            if (this.isFilterSatisfied(filter, data, id)) {
                filteredDataset.push(data);
            }
        }
        if (!filteredDataset) {
            throw  new Error("No satisfied data found.");
        } else {
            return filteredDataset;
        }
    }*/
    /*private trimDatasetByColumns(dataset: any[], columns: string[]): any[] {
        const trimmedDataset: any[] = [];
        for (const data of dataset) {
            const entry: {[key: string]: any } = {};
            for (const ID_KEY of columns) {
                let key;
                if (ID_KEY.includes("_")) {
                    key = ID_KEY.split("_")[1];
                } else {
                    key = ID_KEY;
                }
                if (data.hasOwnProperty(key)) {
                    entry[ID_KEY] = data[key];
                } else {
                    throw new Error("Invalid Key.");
                }
            }
            trimmedDataset.push(entry);
        }
        return trimmedDataset;
    }*/
    public listDatasets(): Promise<InsightDataset[]> { // did myself
        return new Promise<InsightDataset[]> ( (resolve, reject) => {
            let result: InsightDataset[] = [];

            for (let id of this.coursesMap.keys()) {
                let crows: number = this.coursesMap.get(id).length;
                console.log(crows);
                result.push({id, kind: InsightDatasetKind.Courses, numRows: crows});
            }

            resolve(result);
        });
    }
}
