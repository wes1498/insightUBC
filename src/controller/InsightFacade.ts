import Log from "../Util";
import {IInsightFacade, InsightDataset, InsightDatasetKind, } from "./IInsightFacade";
import {JSZipObject} from "jszip";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

const JSZip = require("jszip");
const fs = require("fs");
const COURSES = InsightDatasetKind.Courses;
const courses: Map<string, Course[]> = new Map<string, Course[]>();

export class Course {
    [key: string]: any;
    public dept: string;
    public id: string;
    public avg: number;
    public instructor: string;
    public title: string;
    public pass: number;
    public fail: number;
    public audit: number;
    public uuid: string;
    public year: number;

    constructor (dept: string, id: string, avg: number, instructor: string, title: string, pass: number, fail: number, audit: number, uuid: string, year: number) {
        this.dept = dept;
        this.id = id;
        this.avg = avg;
        this.instructor = instructor;
        this.title = title;
        this.pass = pass;
        this.fail = fail;
        this.audit = audit;
        this.uuid = uuid;
        this.year = year;
    }
}

export interface IDataset {
    id: string;
    data: any[];
}
export default class InsightFacade implements IInsightFacade {

    public loadDatasets: Map<string, IDataset[]>;

    public static insightFacade: InsightFacade = new InsightFacade();

    constructor() {
        Log.trace("InsightFacadeImpl::init()");
        this.loadDatasets = new Map<string, IDataset[]>();
    }
    public static getInstance() {
        return InsightFacade.insightFacade;
    }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        return new Promise(function (resolve, reject) {
            let isDatasetLoaded = false;
            if (this.loadDatasets.has(id)) {
                isDatasetLoaded = true;
            }
            let zip = new JSZip();
            if (kind === InsightDatasetKind.Courses || this.loadedDatasets.has(id)) {
                this.coursesHelper(id, content, zip, isDatasetLoaded);
            } else {
                reject("error occured in handling dataset");
            }
        });
    }

    public  coursesHelper(id: string, content: string, zip: any, isloaded: boolean) {
        zip.loadAsync(content, {base64: true}).then(function (result: any) {
            let format: IDataset = {
                id: "courses",
                data: []
            };
            let promises: Promise<any>[] = [];
            result.forEach(function (relativePath: string, file: JSZipObject) {
                if (file.dir.valueOf() === false) {
                    file.async("text").then(function (results: any) {
                        try {
                            let Jformat = JSON.parse(results);
                            Jformat.result.forEach(function (key: any) {
                                let CourseKeys: Course = new Course(key.dept, key.id, key.number, key.instructor, key.title, key.pass, key.fail, key.audit, key.uuid, key.year);
                                format.data.push(CourseKeys);
                            });
                        } catch (err) {
                            console.log("shit didnt work");
                        }
                    });
                }
            });
        });
    }

    public removeDataset(id: string): Promise<string> {
        return Promise.reject("Not implemented.");
    }

    public performQuery(query: any): Promise <any[]> {
        return Promise.reject("Not implemented.");
    }

    public listDatasets(): Promise<InsightDataset[]> {
        return new Promise<InsightDataset[]>((fulfill, reject) => {
            const result: InsightDataset[] = [];
            for (const id of courses.keys()) {
                const numRows = courses.get(id).length;
                result.push({id, kind: COURSES, numRows});
            }
            const body: InsightDataset[] = result;
            fulfill();
        });
    }
}
