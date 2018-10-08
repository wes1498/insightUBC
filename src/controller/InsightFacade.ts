import Log from "../Util";
import {
    IInsightFacade,
    InsightCourse,
    InsightDataset,
    InsightDatasetKind,
    InsightError,
    InsightFilter,
    NotFoundError,
    InsightStrippedCourse,
} from "./IInsightFacade";
import * as JSZip from "jszip";
import {JSZipObject} from "jszip";
import * as fs from "fs";
import {bodyParser} from "restify";
import {isNegativeNumberLiteral} from "tslint";
import {type} from "os";
import {Decimal} from "decimal.js";
import {fileExists} from "ts-node";
import {isNumber, isString} from "util";
import {currentId} from "async_hooks";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
export default class InsightFacade implements IInsightFacade {

    private coursesMap: Map<string, InsightCourse[]>;

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.coursesMap = new Map<string, InsightCourse[]>();
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
 /*   private addCourse(course: string, datasetId: string) {
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
    }*/
    private addCourse(course: string, datasetId: string) {
        try {
            let parsedCourse = JSON.parse(course);
            const sections: object[] = parsedCourse.result;
            sections.forEach((section: any) => {  // confirm the right type with TA
                let dept: string = section.Subject as string;
                let id: string = section.Course as string;
                let avg: number = section.Avg as number;
                let instructor: string = section.Professor as string;
                let title: string = section.Title as string;
                let pass: number = section.Pass as number;
                let fail: number = section.Fail as number;
                let audit: number = section.Audit as number;
                let uuid: string = section.id.toString() as string;
                let year: number = Number(section.Year);

                let validCourse: InsightCourse = {
                    courses_dept: dept, courses_id: id, courses_avg: avg, courses_instructor: instructor,
                    courses_title: title, courses_pass: pass, courses_fail: fail, courses_audit: audit,
                    courses_uuid: uuid, courses_year: year
                };
                for (let value of Object.values(validCourse)) {
                    if (value === undefined || typeof value === "object" || value instanceof Array) {
                        throw new TypeError("Some of the fields are missing or of non-cast type");
                    }
                }
                this.coursesMap.get(datasetId).push(validCourse);
            });

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
   public performQuery(query: any): Promise<any[]> {
        return new Promise<any[]>((resolve, reject) => {
            try {
                let body: InsightFilter = query.WHERE;
                let options = query.OPTIONS;

                let columns = options.COLUMNS;
                let order = options.ORDER;

                let id: string = columns[0].split("_")[0];

                let datasetId = id;

                // 1. Filter only desired sections and perform syntatic and semantic check as you go
                //    Throw an error if any problems arise, error will be caught in this method and reject
                let result: InsightStrippedCourse[];
                if (Object.keys(body).length === 0) {
                    console.log(datasetId);
                    result = this.coursesMap.get(datasetId);
                    if (result.length > 5000) {
                        throw new InsightError("Too many sections in result"); }
                } else {
                    console.log(datasetId);
                    result = InsightFacade.filterCourses(body, this.coursesMap.get(datasetId));
                }

                // 2. Keep only desired columns by using filteredSections and columns variables
                if (columns && columns.length !== 0) {
                    result = this.keepDesiredColumns(result, columns);
                }
                // 3. if there is optional order field -> sort the filtered and stripped
                if (order !== undefined) {
                    if (columns.includes(order)) {
                        this.orderRows(result, order);
                    } else {
                        throw new InsightError("Order not in selected columns!");
                    }
                }
                // Conclusion: if everything goes without problems resolve with the final dataset
                return resolve(result);
            } catch (e) {
                return reject(new InsightError("Error" + e));
            }

        });
    }

    private keepDesiredColumns(filteredSections: any, columns: any[]): InsightStrippedCourse[] {
        let strippedResult: object[] = [];
        filteredSections.forEach((section: any) => {
            let strippedSection: InsightStrippedCourse = {};
            columns.forEach((key) => {
                if (InsightFacade.isValidKey(key)) {
                    strippedSection[key] = section[key];
                }
            });
            strippedResult.push(strippedSection);
        });
        return strippedResult;
    }

    private static isValidKey(key: string): boolean {
        switch (key) {
            case "courses_dept":
                return true;
            case "courses_id":
                return true;
            case "courses_instructor":
                return true;
            case "courses_title":
                return true;
            case "courses_uuid":
                return true;
            case "courses_avg":
                return true;
            case "courses_pass":
                return true;
            case "courses_fail":
                return true;
            case "courses_audit":
                return true;
            case "courses_year":
                return true;
            default:
                return false;
        }
    }
    // order rows by key
    // fix for string
    private orderRows(result: InsightStrippedCourse[], order: any): any[] {
        return result.sort( (a, b) => {
            let x = a[order];
            let y = b[order];
            return ((x < y) ? -1 : ((x > y) ? 1 : 0));
        });
    }

    // For every course section in toQuery, check if filter applies
    private static filterCourses(filter: InsightFilter, toQuery: InsightCourse[]): InsightCourse[] {
        let result: InsightCourse[] = [];
        // console.log(toQuery)
        for (let section of toQuery) {
            if (InsightFacade.isSectionValid(filter, section)) {
                result.push(section);
            }
        }
        if (result.length > 5000) {
            throw new InsightError("Result exceeds 5000 limit");
        }
        return result;
    }

    private static hasLogic(comparison: any) {
        return comparison.hasOwnProperty("AND") || comparison.hasOwnProperty("OR");
    }

    private static hasMCOMComparison(comparison: any) {
        return comparison.hasOwnProperty("LT") || comparison.hasOwnProperty("GT") || comparison.hasOwnProperty("EQ");
    }

    private static hasSComparison(comparison: any) {
        return comparison.hasOwnProperty("IS");
    }

    private static hasNegation(comparison: any) {
        return comparison.hasOwnProperty("NOT");
    }

    private static validateMCOMFilter(filter: InsightFilter) {
        let bodyOfMComparison = Object.values(filter)[0];
        if (typeof Object.values(bodyOfMComparison)[0] !== "number") {
            throw new InsightError("Invalid value type for MCOMFilter");
        }
        if (!this.isValidKey(Object.keys(bodyOfMComparison)[0])) {
            throw new InsightError("Invalid key: MCOM");
        }
        switch (Object.keys(bodyOfMComparison)[0]) {
            case "courses_dept":
                throw new InsightError("Invalid key: IS");
            case "courses_id":
                throw new InsightError("Invalid key: IS");
            case "courses_instructor":
                throw new InsightError("Invalid key: IS");
            case "courses_title":
                throw new InsightError("Invalid key: IS");
            case "courses_uuid":
                throw new InsightError("Invalid key: IS");
        }
    }

    private static handleLT(lt: object, section: { [key: string]: number }): boolean {
        let keyToCheck: string = Object.keys(lt)[0];
        let valueToCheck: number = Object.values(lt)[0];
        let actualValue: number = section[keyToCheck];
        return actualValue < valueToCheck;
    }

    private static handleGT(gt: object, section: { [key: string]: number }): boolean {
        let keyToCheck: string = Object.keys(gt)[0];
        let valueToCheck: number = Object.values(gt)[0];
        let actualValue: number = section[keyToCheck];
        return actualValue > valueToCheck;
    }

    private static handleEQ(eq: object, section: { [key: string]: number }): boolean {
        let keyToCheck: string = Object.keys(eq)[0];
        let valueToCheck: number = Object.values(eq)[0];
        let actualValue: number = section[keyToCheck];
        return actualValue === valueToCheck;
    }

    private static handleSComparison(is: object, section: { [key: string]: string }): boolean {
        let value: any = Object.values(is)[0];
        if (typeof value !== "string") {
            throw new InsightError("Invalid type: IS");
        }
        let key: any = Object.keys(is)[0];
        if (!this.isValidKey(key)) {
            throw new InsightError("Invalid key: IS");
        }
        switch (key) {
            case "courses_avg":
                throw new InsightError("Invalid key: IS");
            case "courses_pass":
                throw new InsightError("Invalid key: IS");
            case "courses_fail":
                throw new InsightError("Invalid key: IS");
            case "courses_audit":
                throw new InsightError("Invalid key: IS");
            case "courses_year":
                throw new InsightError("Invalid key: IS");
        }
        let actual: string = section[key];
        if (value.includes("*")) {
            if (value.length === 1) {
                return value === "*";
            } else if (value.startsWith("*") && value.endsWith("*")) {
                return actual.includes(value.substring(1, value.length - 1));
            } else if (value.startsWith("*")) {
                return actual.endsWith(value.substring(1));
            } else if (value.endsWith("*")) {
                return actual.startsWith(value.substring(0, value.length - 1));
            } else {
                throw new InsightError("Asteriks cannot be in the middle");
            }
        }
        return value === actual;
    }

    // Check if filter applies to given section
    private static isSectionValid(filter: InsightFilter, section: any): boolean {
        if (InsightFacade.hasLogic(filter)) {

            if (filter.hasOwnProperty("AND")) {
                let toAnd: any[] = filter.AND;
                if (toAnd.length === 0 ) {
                    throw new InsightError("Not enough conditions for AND");
                }

                // for each filter section must be valid
                // recursive call to each filter in the array on the section
                for (let subfilter of toAnd) {
                    if (!this.isSectionValid(subfilter, section)) {
                        return false;
                    }
                }
                return true;

            } else {
                let toOr: any[] = filter.OR;
                if (toOr.length === 0) {
                    throw new InsightError("Not enough conditions for OR");
                }
                // for at least one filter section must be valid:
                // recursive call to each filter in the array on the section
                for (let subfilter of filter.OR) {
                    if (this.isSectionValid(subfilter, section)) {
                        return true;
                    }
                }
                return false;
            }

        } else if (InsightFacade.hasMCOMComparison(filter)) {

            this.validateMCOMFilter(filter);

            if (filter.hasOwnProperty("LT")) {

                return InsightFacade.handleLT(filter.LT, section);

            } else if (filter.hasOwnProperty("GT")) {

                return InsightFacade.handleGT(filter.GT, section);

            } else {

                return InsightFacade.handleEQ(filter.EQ, section);

            }

        } else if (InsightFacade.hasSComparison(filter)) {

            return InsightFacade.handleSComparison(filter.IS, section);

        } else if (InsightFacade.hasNegation(filter)) {

            // recursively call the same function with the same inputs but negated
            return (!this.isSectionValid(filter.NOT, section));

        } else {
            if (Object.keys(filter).length === 0 && filter.constructor === Object) {
                return false;
            } else {
                throw new InsightError("Did not match any of the keys");
            }
        }
    }
   public listDatasets(): Promise<InsightDataset[]> { // did myself
        return new Promise<InsightDataset[]> ( (resolve, reject) => {
            let result: InsightDataset[] = [];

            for (let id of this.coursesMap.keys()) {
                let crows: number = this.coursesMap.get(id).length;
               // console.log(crows);
                result.push({id, kind: InsightDatasetKind.Courses, numRows: crows});
            }

            resolve(result);
        });
    }
}
