import { z } from 'zod';

export const getRolesSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  searchQuery: z.string().optional(),
  first: z.number(),
  rows: z.number(),
  filters: z.any().optional(),
  is_enabled: z.boolean().optional(),
});
export const getRolesPermisionSchema = z.object({
  startDate: z.date().optional(),
  endDate: z.date().optional(),
  id: z.number().optional().nullable(),
  searchQuery: z.string().optional(),
  first: z.number(),
  rows: z.number(),
  filters: z.any().optional(),
  is_enabled: z.boolean().optional(),
});
export type getRolesFilterSchema = z.TypeOf<typeof getRolesSchema>;
export type getRolesPermisionFilterSchema = z.TypeOf<
  typeof getRolesPermisionSchema
>;

export const addResourcesSchema = z.object({
  id: z.number().optional().nullable(),

  name: z
    .string({
      required_error: ' please Enter type',
    })
    .min(1, {
      message: 'please Enter type',
    }),
  code: z
    .string({
      required_error: ' please Enter code',
    })
    .min(1, {
      message: 'please Enter code',
    }),
});
export const addPermisionSchema = z.array(
  z.object({
    access: z
      .enum(['W', 'R', 'N'], {
        required_error: ' please select access type',
      })
      .default('N'),
    resource_id: z.number().default(0),
    role_id: z.number().default(0),
  }),
);
export const deleteCouponSchema = z.object({
  id: z.number(),
});

export const updateSchema = z.object({
  user_id: z.number(),
  coupon_id: z.number().optional(),
  name: z.string().trim(),
  coupon_code: z
    .string()
    .max(6, {
      message: 'Coupon Code must be at least 6 characters',
    })
    .max(6, {
      message: 'Coupon Code must be at least 6 characters',
    })
    .trim()
    .refine((s) => !s.includes(' '), `Please don't use spaces`),
  is_percentage: z.string(),
  is_limited: z.string(),
  coupon_limit: z.number().optional(),
  discount: z.number(),
  end_date: z.date(),
});
export const updateCouponSchema = z.object({
  coupon_id: z.number(),
  is_enabled: z.boolean().optional(),
});

export type updateCouponType = z.infer<typeof updateCouponSchema>;
export type addResourceInput = z.TypeOf<typeof addResourcesSchema>;
