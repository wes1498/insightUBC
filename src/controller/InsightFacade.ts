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
    public performQuery(query: any): Promise <any[]> {
        return new Promise<any[]>(function (resolve, reject) {
            try {
                let filter: InsightFilter = query.WHERE;
                let options = query.OPTIONS;
                // let transformations = query.TRANSFORMATIONS;
                let columns = options.COLUMNS;
                let order = options.ORDER;
                let id: string = columns[0].split("_")[0];
                let dataset = "exactlyOneValid";

                let result: any;
                // Use filter (LT|GT|EQ) to find course sections
                if (Object.keys(filter).length > 0) {
                    result = InsightFacade.filterCoursesHelper(filter, this.courseMap.get(dataset));
                    // this returns all sections if no filter (LT|GT|EQ)
                } else if (Object.keys(filter).length === 0) {
                    result = this.courseMap.get(dataset);
                    if (result.length > 5000) {
                        throw new InsightError("Too many sections in result");
                    }
                }
                // Manage the COLUMNS -- filter correct sections
                if ((columns.length !== 0) && columns) {
                    let finalResult: object[] = [];
                    result.forEach(function (section: any) {
                        let finalSection: any = {};
                        columns.forEach(function (key: any) {
                            if (this.InsightFacade.validKeyHelper(key)) {
                                finalSection[key] = section[key];
                            }
                        });
                        finalResult.push(finalSection);
                    });
                    result = finalResult;
                }
                // Manage the ORDER
                if (order !== undefined && order !== null) {
                    // if ORDER is required
                    if (columns.includes(order)) {
                        this.rowOrderHelper(result, order);
                    } else {
                        throw new InsightError("ORDER not in COLUMNS");
                    }
                }
                // resolve if all goes well
                return resolve(result);
            } catch (err) {
                return reject(new InsightError("DWhack"));
            }
        });
    }

    // For every course section in toQuery, check if filter applies
    private static filterCoursesHelper(logic: InsightFilter, Query: InsightCourse[]): InsightCourse[] {
        let result: InsightCourse[] = [];
        for (let courseSection of Query) {
            if (InsightFacade.isSectionValidHelper(logic, courseSection)) {
                result.push(courseSection);
            }
        }
        if (result.length <= 5000) {
            return result;
        } else {
            throw new InsightError("5000 limit exeeded");
        }
    }
    // determine if the key format is valid and exists
    private static validKeyHelper(key: string): boolean {
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

    // ORDER rows by key
    private rowOrderHelper(result: any, order: any): any[] {
        return result.sort( function (a: any, b: any) {
            let x = a[order];
            let y = b[order];
            return ((x < y) ? -1 : ((x > y) ? 1 : 0));
        });
    }
    // logic properties
    private static hasLOGICHelper(compare: any) {
        if (compare.hasOwnProperty("AND") || compare.hasOwnProperty("OR")) {
            return true;
        } else {
            return false;
        }
    }
    // MCOMPARATOR properties
    private static hasMCOMPARATORHelper(compare: any) {
        if (compare.hasOwnProperty("GT") || compare.hasOwnProperty("EQ") || compare.hasOwnProperty("LT")) {
            return true;
        } else {
            return false;
        }
    }
    // string comparison properties
    private static hasSComparisonHelper(compare: any) {
        if (compare.hasOwnProperty("IS")) {
            return true;
        } else {
            return false;
        }
    }
    // negation properties needed
    private static hasNegationHelper(compare: any) {
        if (compare.hasOwnProperty("NOT")) {
            return true;
        } else {
            return false;
        }
    }

    private static validateMCOMFilter(filter: InsightFilter) {
        // MComparison object with property filter
        let keyProp = Object.values(filter)[0];
        // if valid key is not a number, it is invalid
        if (typeof Object.values(keyProp)[0] !== "number") {
            throw new InsightError("key value is not a number");
        }
        // if not a valid key
        if (!this.validKeyHelper(Object.keys(keyProp)[0])) {
            throw new InsightError("Invalid key: MCOM");
        }
        // check key validity to filter
        switch (Object.keys(keyProp)[0]) {
            case "courses_dept":
                throw new InsightError("Invalid key");
            case "courses_id":
                throw new InsightError("Invalid key");
            case "courses_instructor":
                throw new InsightError("Invalid key");
            case "courses_title":
                throw new InsightError("Invalid key");
            case "courses_uuid":
                throw new InsightError("Invalid key");
        }
    }

    private static handleLTHelper(lt: object, section: { [key: string]: number }): boolean {
        let checkKey: string = Object.keys(lt)[1];
        let valueCheck: number = Object.values(lt)[1];
        let actualValue: number = section[checkKey];
        if (actualValue < valueCheck) {
            return true;
        } else {
            return false;
        }
    }

    private static handleGTHelper(gt: object, section: { [key: string]: number }): boolean {
        let checkKey: string = Object.keys(gt)[1];
        let valueCheck: number = Object.values(gt)[1];
        let actualValue: number = section[checkKey];
        if (actualValue > valueCheck) {
            return true;
        } else {
            return false;
        }
    }

    private static handleEQ(eq: object, section: { [key: string]: number }): boolean {
        let checkKey: string = Object.keys(eq)[1];
        let valueCheck: number = Object.values(eq)[1];
        let actualValue: number = section[checkKey];
        if (actualValue === valueCheck) {
            return true;
        } else {
            return false;
        }
    }

    private static handleSComparisonHelper(is: object, section: { [key: string]: string }): boolean {
        let value: any = Object.values(is)[0];
        // if value is not of type string
        if (typeof value !== "string") {
            throw new InsightError("Invalid value");
        }
        // if key is not valid
        let key: any = Object.keys(is)[0];
        if (!this.validKeyHelper(key)) {
            throw new InsightError("Invalid key");
        }
        // check which keys are valid to use for IS comparison
        switch (key) {
            // cannot use any key of value type number!
            case "courses_year":
                throw new InsightError("Invalid key");
            case "courses_avg":
                throw new InsightError("Invalid key");
            case "courses_fail":
                throw new InsightError("Invalid key");
            case "courses_audit":
                throw new InsightError("Invalid key");
            case "courses_pass":
                throw new InsightError("Invalid key");
        }
        let actual: string = section[key];
        // check for wildcards
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
    private static isSectionValidHelper(filter: InsightFilter, section: any): boolean {
        if (InsightFacade.hasLOGICHelper(filter)) {

            if (filter.hasOwnProperty("AND")) {
                // AND must compare 2 things
                let AndComp: any[] = filter.AND;
                // AND must compare 1 or more
                if (AndComp.length === 0 ) {
                    throw new InsightError("Not enough conditions for AND");
                }

                // for each filter section must be valid
                // recursive call to each filter in the array on the section
                for (let insideFilter of AndComp) {
                    if (!this.isSectionValidHelper(insideFilter, section)) {
                        return false;
                    }
                }
                return true;

            } else {
                // OR must compare 2 things
                let OrComp: any[] = filter.OR;
                // OR must compare 1 or more
                if (OrComp.length === 0) {
                    throw new InsightError("Not enough conditions for OR");
                }
                // for at least one filter section must be valid:
                // recursive call to each filter in the array on the section
                for (let insideFilter of filter.OR) {
                    if (this.isSectionValidHelper(insideFilter, section)) {
                        return true;
                    }
                }
                return false;
            }

        } else if (InsightFacade.hasMCOMPARATORHelper(filter)) {

            this.validateMCOMFilter(filter);

            if (filter.hasOwnProperty("LT")) {

                return InsightFacade.handleLTHelper(filter.LT, section);

            } else if (filter.hasOwnProperty("GT")) {

                return InsightFacade.handleGTHelper(filter.GT, section);

            } else {

                return InsightFacade.handleEQ(filter.EQ, section);

            }

        } else if (InsightFacade.hasSComparisonHelper(filter)) {

            return InsightFacade.handleSComparisonHelper(filter.IS, section);

        } else if (InsightFacade.hasNegationHelper(filter)) {

            // recursively call the same function with the same inputs but negated
            return (!this.isSectionValidHelper(filter.NOT, section));

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
