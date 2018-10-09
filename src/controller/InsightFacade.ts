import Log from "../Util";
import {
    IInsightFacade,
    InsightCourse,
    InsightDataset,
    InsightDatasetKind,
    InsightError,
    InsightFilter,
    NotFoundError,
    InsightCourseEmpty,
} from "./IInsightFacade";
import * as JSZip from "jszip";
import {JSZipObject} from "jszip";
import * as fs from "fs";
import {bodyParser} from "restify";
import {isNegativeNumberLiteral} from "tslint";
import {type} from "os";
import {Decimal} from "decimal.js";
import {fileExists} from "ts-node";
import {isNumber, isString, log} from "util";
import {currentId} from "async_hooks";
import ProcessEnv = NodeJS.ProcessEnv;
import * as Path from "path";

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
                }
                let filesPromise: any[] = [];
                if (unzipped.length < 1) {
                    return reject(new InsightError("Invalid Id"));
                } else if (unzipped.files.hasOwnProperty(dataType) || unzipped.length > 0) {
                    unzipped.forEach(((relativePath, fileObject: JSZipObject) => {
                        // if (fileObject.dir) {
                        // return reject(new InsightError("Is a folder "));
                        //  } else {
                        filesPromise.push(fileObject.async("text").then((data: string) => {
                            this.addCourse(data, id); // save the course info a
                        }).catch((e) => {
                            return reject(new InsightError("Error processing encoded course data " + e));
                        }));
                        // }
                    }));
                } else {
                    return reject(new InsightError("Desired folder for the dataset kind does not exist"));
                }
                Promise.all(filesPromise).then(() => {
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
            let parsedInfo = JSON.parse(course);
            if ((parsedInfo.result.length === 0)) {
                throw new Error("0 section.");
            }
            const sections: object[] = parsedInfo.result;
            sections.forEach((section: any) => {
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
                this.coursesMap.get(datasetId).push(validCourse);
            });

        } catch (e) {
            // not in JSON format or some fields of different type/missing-> skip this course
        }
    }
    private pathReader(folderName: string): void { // deprecated
        let that = this;
        fs.readdir(folderName, function (e , files) {
            if (e) {
                let error1 =  "Error reading dir";
                return error1;
            } else {
                files.forEach(function (filesId) {
                    // dataset already loaded
                    if (that.coursesMap.has(filesId)) {
                        return;
                    } else {
                        let name = folderName + filesId;
                        fs.readFile(name, "utf-8", function (e2, content: string) {
                            if (e2) {
                                return;
                            }
                            that.coursesMap.set(filesId, JSON.parse(content));
                        });
                    }
                });
            }
        });
    }
    public removeDataset(id: string): Promise<string> {// no changes needed
        return new Promise<string>((resolve, reject) => {
            if (id === "") {
                return reject(new NotFoundError("Invalid ID"));
            } else if (id === null || !id) {
                return reject(new InsightError("Invalid ID"));
            } else if (!this.coursesMap.has(id)) {
                return reject(new NotFoundError("Not found"));
            }
            if (this.coursesMap.has(id)) {
                this.removeFromMemory(id, InsightDatasetKind.Courses).then((succ) => {
                    return resolve(id);
                }).catch((err) => {
                    return reject(new NotFoundError("error :" +  err ));
                });
            } else if (!this.coursesMap.has(id)) {
                return reject(new NotFoundError("Not found"));
            }
            this.coursesMap.delete(id);
            return resolve (id);
        });
    }
    private removeFromMemory(id: string, kind: InsightDatasetKind): Promise<boolean> {
        return new Promise<boolean>((resolve, reject) => {
            fs.readdir(kind.toString(), (err, files) => {
                if (err) {
                    // console.log(err + "1");
                    reject(false);
                } else {
                    if (files.includes(id)) {
                        const pathy = Path.join(kind, id);
                        fs.unlink(pathy, (err2) => {
                            if (err2) {
                               //  console.log(err2);
                                reject(false);
                            } else {
                                resolve(true);
                            }
                        });
                    } else {
                        reject(false);
                    }
                }
            });
        });
    }
    public performQuery(query: any): Promise<any[]> {
        return new Promise<any[]>((resolve, reject) => {
            try {
                let filter: InsightFilter = query.WHERE;
                let options = query.OPTIONS;
                let order = options.ORDER;
                let columns = options.COLUMNS;
                let id: string = columns[0].split("_")[0];

                let dataset = id;

                // perform syntax checks as you go

                let result: InsightCourseEmpty[];
                if (Object.keys(filter).length === 0) {
                    // console.log(dataset);
                    result = this.coursesMap.get(dataset);
                   // console.log(this.coursesMap.get(dataset));
                    if (result.length > 5000) {
                        throw new InsightError("Too many sections in result"); }
                } else {
                    // console.log(dataset);
                    result = InsightFacade.filterCourses(filter, this.coursesMap.get(dataset));
                }

                // keep only the desired columns in query
                if (columns && columns.length !== 0) {
                    let columnResult: object[] = [];
                    let filteredSections: any = result;
                    filteredSections.forEach( function (section: any) {
                        let columnSection: InsightCourseEmpty = {};
                        columns.forEach( function (key: any) {
                            if (InsightFacade.validKeyHelper(key)) {
                                columnSection[key] = section[key];
                            }
                        });
                        columnResult.push(columnSection);
                    });
                    result = columnResult;
                    // result = this.desiredColumnsHelper(result, columns);
                }
                // Sort the result if order is included
                if (order !== undefined || order !== null) {
                    if (columns.includes(order)) {
                        result = result.sort( function (a, b) {
                            let x = a[order];
                            let y = b[order];
                            if (x === y) {
                                return 0;
                            } else if (x > y ) {
                                return 1;
                            } else {
                                return -1;
                            }
                        });
                        // this.rowOrderHelper(result, order);
                    } else {
                        throw new InsightError("ORDER not in COLUMNS");
                    }
                }
                // resolve if no problems
                return resolve(result);
            } catch (e) {
                return reject(new InsightError("Error in reading query"));
            }

        });
    }
    private static validKeyHelper(key: string): boolean {
        // check if the key being passed is a valid one
        switch (key) {
            case "courses_dept":
                return true;
            case "courses_id":
                return true;
            case "courses_instructor":
                return true;
            case "cpurses_title":
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
    // For every course section in Query, check if filter applies a
    private static filterCourses(filter: InsightFilter, Query: InsightCourse[]): InsightCourse[] {
        let result: InsightCourse[] = [];
        // console.log(Query)
        for (let section of Query) {
            // check if you can apply filter to the key
            if (InsightFacade.isSectionValid(filter, section)) {
                result.push(section);
            }
        }
        // result over 5000 to add
        if (result.length > 5000) {
            throw new InsightError("Result exceeds 5000 limit");
        }
        return result;
    }

    private static hasLogicCompHelper(comparison: any) {
        if (comparison.hasOwnProperty("AND") || comparison.hasOwnProperty("OR")) {
            return true;
        } else {
            return false;
        }
    }

    private static hasMComparatorHelper(comparison: any) {
        if (comparison.hasOwnProperty("GT") || comparison.hasOwnProperty("LT") || comparison.hasOwnProperty("EQ")) {
            return true;
        } else {
            return false;
        }
    }

    private static hasSComparisonHelper(comparison: any) {
        if (comparison.hasOwnProperty("IS")) {
            return true;
        } else {
            return false;
        }
    }

    private static hasNegationHelper(comparison: any) {
        if (comparison.hasOwnProperty("NOT")) {
            return true;
        } else {
            return false;
        }
    }

    private static validateMComFilterHelper(filter: InsightFilter) {
        let body = Object.values(filter)[0];
        // MCOMPARATOR must be a number
        // check for valid value
        if (typeof Object.values(body)[0] !== "number") {
            throw new InsightError("Invalid value");
        }
        // check for valid key
        if (!this.validKeyHelper(Object.keys(body)[0])) {
            throw new InsightError("Invalid key");
        }
        // throw error for any non-number key
        if (Object.keys(body)[0] === "courses_dept") {
            throw new InsightError("Not valid key");
        } else if ((Object.keys(body)[0]) === "courses_id") {
            throw new InsightError("Not valid key");
        } else if ((Object.keys(body)[0]) === "courses_instructor") {
            throw new InsightError("Not valid key");
        } else if ((Object.keys(body)[0]) === "courses_title") {
            throw new InsightError("Not valid key");
        } else if ((Object.keys(body)[0]) === "courses_uuid") {
            throw new InsightError("Not valid key");
        }
    }

    // best move is to SPLIT into helpers for each identifiable filter
    private static handleLTHelper(lt: object, section: { [key: string]: number }): boolean {
        if (section[Object.keys(lt)[0]] < Object.values(lt)[0]) {
            return true;
        } else {
            return false;
        }
    }

    private static handleGTHelper(gt: object, section: { [key: string]: number }): boolean {
        if (section[Object.keys(gt)[0]] > Object.values(gt)[0]) {
            return true;
        } else {
            return false;
        }
    }

    private static handleEQHelper(eq: object, section: { [key: string]: number }): boolean {
        if (section[ Object.keys(eq)[0]] === Object.values(eq)[0]) {
            return true;
        } else {
            return false;
        }
    }

    private static handleSComparisonHelper(is: object, section: { [key: string]: string }): boolean {

        let key: any = Object.keys(is)[0];
        // check if key is not invalid
        if (!this.validKeyHelper(key)) {
            throw new InsightError("Invalid key");
        }
        let value: any = Object.values(is)[0];
        // check if value is of right type
        if (typeof value !== "string") {
            throw new InsightError("Invalid type");
        }
        if (key === "courses_avg") {
            throw new InsightError("Not valid key");
        } else if (key === "courses_pass") {
            throw new InsightError("Not valid key");
        } else if (key === "courses_fail") {
            throw new InsightError("Not valid key");
        } else if (key === "courses_year") {
            throw new InsightError("Not valid key");
        } else if (key === "courses_audit") {
            throw new InsightError("Not valid key");
        }
        let actualRes: string = section[key];
        // check each wildcard case
        if (value.includes("*")) {
            if (value.length === 1) {
                return value === "*";
                // *ell*, *ello, hell*
            } else if (value.length === 2 && value.startsWith("**")) {
                return value === "**";
            } else if (value.startsWith("**") || value.endsWith("**")) {
                throw new InsightError("Asteriks cannot be in the middle");
            } else if (value.startsWith("*") && value.endsWith("**")) {
                throw new InsightError("Asteriks cannot be in the middle");
            } else if (value.startsWith("**") && value.endsWith("*")) {
                throw new InsightError("Asteriks cannot be in the middle");
            } else if (value.startsWith("*") && value.endsWith("*")) {
                return actualRes.includes(value.substring(1, value.length - 1));
            } else if (value.startsWith("*")) {
                return actualRes.endsWith(value.substring(1));
            } else if (value.endsWith("*")) {
                return actualRes.startsWith(value.substring(0, value.length - 1));
            } else {
                // h**lo or h*llo === error
                throw new InsightError("Asteriks cannot be in the middle");
            }
        }
        return value === actualRes;
    }

    // Check if filter applies to given section
    private static isSectionValid(filter: InsightFilter, section: any): boolean {
        if (InsightFacade.hasLogicCompHelper(filter)) {
            // check AND COMP
            if (filter.hasOwnProperty("AND")) {
                // AND must be 1 or more
                if (filter.AND.length === 0 ) {
                    throw new InsightError("Not enough conditions for AND");
                }

                // for each filter section must be valid
                // recursive call to each filter in the array on the section
                for (let insideFilter of filter.AND) {
                    if (!this.isSectionValid(insideFilter, section)) {
                        return false;
                    }
                }
                // if all filters valid on each section
                return true;

            } else {
                // OR must be 1 or more
                if (filter.OR.length === 0) {
                    throw new InsightError("Not enough conditions for OR");
                }
                // for at least one filter section must be valid:
                // recursive call to each filter in the array on the section
                for (let insideFilter of filter.OR) {
                    if (this.isSectionValid(insideFilter, section)) {
                        return true;
                    }
                }
                return false;
            }
            // check if it is an MCOMPARATOR
        } else if (InsightFacade.hasMComparatorHelper(filter)) {

            this.validateMComFilterHelper(filter);

            if (filter.hasOwnProperty("GT")) {

                return InsightFacade.handleGTHelper(filter.GT, section);

            } else if (filter.hasOwnProperty("LT")) {

                return InsightFacade.handleLTHelper(filter.LT, section);

            } else {

                return InsightFacade.handleEQHelper(filter.EQ, section);

            }
        } else if (InsightFacade.hasNegationHelper(filter)) {

            // recursively call the same function with the same inputs but negated
            return (!this.isSectionValid(filter.NOT, section));

            // Check if it is an SComparison
        } else if (InsightFacade.hasSComparisonHelper(filter)) {

            return InsightFacade.handleSComparisonHelper(filter.IS, section);

        } else {
            // if no there is no filter
            if (filter.constructor === Object && Object.keys(filter).length === 0) {
                return false;
            } else {
                throw new InsightError("Did not match any of the keys");
            }
        }
    }
    public listDatasets(): Promise<InsightDataset[]> {
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
