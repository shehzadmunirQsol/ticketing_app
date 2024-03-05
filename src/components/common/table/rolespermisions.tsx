import React, { useEffect, useState } from 'react';
import {
  ColumnDef,
  // ColumnFiltersState,
  SortingState,
  VisibilityState,
  flexRender,
  getCoreRowModel,
  getFilteredRowModel,
  getPaginationRowModel,
  getSortedRowModel,
  useReactTable,
} from '@tanstack/react-table';

import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';

import { trpc } from '~/utils/trpc';
import { customEmailTruncateHandler } from '~/utils/helper';
import { getCustomerSchema } from '~/schema/customer';
import {
  Tooltip,
  TooltipContent,
  TooltipProvider,
  TooltipTrigger,
} from '~/components/ui/tooltip';
import { Switch } from '~/components/ui/switch';
import { useToast } from '~/components/ui/use-toast';
import { ScrollArea, ScrollBar } from '~/components/ui/scroll-area';

import { LoadingDialog } from '../modal/loadingModal';

import { useRouter } from 'next/router';
import { Button } from '~/components/ui/button';
export type Roles = {
  id: number;
  name: string;
  code: string;

  is_deleted: boolean;
  created_at: Date;
};

const initialFilters: any = {
  first: 0,
  rows: 50,
};

export default function RolesPermisionDataTable() {
  // use toast
  const { toast } = useToast();
  const router = useRouter();
  const [sorting, setSorting] = useState<SortingState>([]);

  const [filters, setFilters] = useState<getCustomerSchema>(initialFilters);
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});
  const [permissions, setPermissions] = useState<any>({});

  const { index } = router.query;
  // APi
  const { data, refetch, isLoading } =
    trpc.roles.getResourcesPermisions.useQuery(
      { ...filters, id: index ? +index : 0 },
      {
        refetchOnWindowFocus: false,
      },
    );
  const uploadPermisions = trpc.roles.uploadPermisions.useMutation({
    onSuccess: (res) => {
      console.log(res);
    },
  });
  const rolesData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);

  console.log({ permissions }, 'data to get');
  useEffect(() => {
    setPermissions(typeof data?.switch === 'object' ? data?.switch : {});
  }, [data]);

  // delete product

  const ChangePermisions = (name: number, type: string) => {
    console.log({ name, type });
    setPermissions((prev: any) => {
      return {
        ...prev,
        [name]: type,
      };
    });
  };
  // columns
  const columns: ColumnDef<Roles>[] = [
    {
      accessorKey: 'name',
      header: 'Name',
      cell: ({ row }) => {
        return (
          <div className="flex items-center gap-4 text-ellipsis whitespace-nowrap overflow-hidden">
            <TooltipProvider>
              <Tooltip>
                <TooltipTrigger>
                  {customEmailTruncateHandler(row?.original?.name)}
                </TooltipTrigger>
                <TooltipContent>
                  <p className="text-base font-normal">{row?.original?.name}</p>
                </TooltipContent>
              </Tooltip>
            </TooltipProvider>
          </div>
        );
      },
    },
    {
      accessorKey: 'Write',
      header: 'Write',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          <Switch
            checked={permissions[row?.original?.id] === 'W'}
            onCheckedChange={() => ChangePermisions(row?.original?.id, 'W')}
          />
        </div>
      ),
    },
    {
      accessorKey: 'Read',
      header: 'Read',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          <Switch
            checked={permissions[row?.original?.id] === 'R'}
            onCheckedChange={() => ChangePermisions(row?.original?.id, 'R')}
          />
        </div>
      ),
    },
    {
      accessorKey: 'No Access',
      header: 'No Access',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          <Switch
            checked={permissions[row?.original?.id] == 'N'}
            onCheckedChange={() => ChangePermisions(row?.original?.id, 'N')}
          />
        </div>
      ),
    },
  ];

  const table = useReactTable({
    data: rolesData as Roles[],
    columns,
    onSortingChange: setSorting,
    getCoreRowModel: getCoreRowModel(),
    getPaginationRowModel: getPaginationRowModel(),
    getSortedRowModel: getSortedRowModel(),
    getFilteredRowModel: getFilteredRowModel(),
    onColumnVisibilityChange: setColumnVisibility,
    onRowSelectionChange: setRowSelection,
    state: {
      sorting,
      columnVisibility,
      rowSelection,
    },
  });
  const onSubmit = async () => {
    try {
      const arrayOfObj: any = Object.entries(permissions).map((e) => ({
        resource_id: e[0] ? +e[0] : 0,
        access: e[1] ?? 'N',
        role_id: index ? +index : 0,
      })) ?? [
        {
          resource_id: 0,
          role_id: 0,
          access: 'N',
        },
      ];
      const data = await uploadPermisions.mutateAsync([...arrayOfObj]);

      console.log('permissions', data);
      toast({
        variant: 'success',
        title: 'Address Updated Successfully',
      });
      // refetch();
    } catch (e: any) {
      toast({
        variant: 'success',
        title: e?.message ?? 'Something Went Wrong!',
      });
    }
  };

  return (
    <div className="w-full space-y-4 mt-4">
      <div className="flex items-center justify-between mb-4">
        <div className=" text-4xl font-semibold">Roles Permision</div>

        <Button
          type="submit"
          variant={'clip'}
          onClick={() => onSubmit()}
          className="w-fit"
        >
          Save Permisions
        </Button>
      </div>
      <div className="rounded-md border border-border ">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>

          <Table className="w-full overflow-x-scroll">
            <TableHeader className="bg-secondary/80">
              {table?.getHeaderGroups().map((headerGroup) => (
                <TableRow key={headerGroup.id}>
                  {headerGroup.headers.map((header) => {
                    return (
                      <TableHead key={header.id}>
                        {header.isPlaceholder
                          ? null
                          : flexRender(
                              header.column.columnDef.header,
                              header.getContext(),
                            )}
                      </TableHead>
                    );
                  })}
                </TableRow>
              ))}
            </TableHeader>
            <TableBody>
              {table?.getRowModel()?.rows?.length ? (
                table?.getRowModel()?.rows?.map((row) => (
                  <TableRow
                    key={row.id}
                    data-state={row.getIsSelected() && 'selected'}
                  >
                    {row.getVisibleCells().map((cell) => (
                      <TableCell key={cell.id}>
                        {flexRender(
                          cell.column.columnDef.cell,
                          cell.getContext(),
                        )}
                      </TableCell>
                    ))}
                  </TableRow>
                ))
              ) : (
                <TableRow>
                  <TableCell
                    colSpan={columns.length}
                    className="h-24 text-center"
                  >
                    No results.
                  </TableCell>
                </TableRow>
              )}
            </TableBody>
          </Table>
        </ScrollArea>
      </div>

      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
