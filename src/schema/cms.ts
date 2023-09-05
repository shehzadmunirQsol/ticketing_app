import { z } from 'zod';

export const cmsSchema = z.object({
    detail:z.string()
});

export const getCmsSchema = z.object({});

