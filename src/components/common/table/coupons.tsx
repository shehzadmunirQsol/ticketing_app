import React, { useState } from 'react';
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
import { ChevronDown } from 'lucide-react';

import { Button } from '@/ui/button';
import {
  DropdownMenu,
  DropdownMenuCheckboxItem,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuLabel,
  DropdownMenuSeparator,
  DropdownMenuSubContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown-menu';

import {
  Table,
  TableBody,
  TableCell,
  TableHead,
  TableHeader,
  TableRow,
} from '@/ui/table';

import { trpc } from '~/utils/trpc';
import { customEmailTruncateHandler, displayDate } from '~/utils/helper';
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
import { CouponDialog } from '../modal/coupon';
import { LoadingDialog } from '../modal/loadingModal';
import { MoreHorizontal } from 'lucide-react';
import Link from 'next/link';

export type Category = {
  id: number;
  is_enabled: boolean;
  name: string;
  coupon_code: string;
  discount: number;
  coupon_limit: number;

  is_limited: boolean;
  is_percentage: boolean;
  created_at: Date;
  start_date: Date;
  end_date: Date;
  updated_at: Date;
};

export default function CouponsDataTable() {
  // use toast
  const { toast } = useToast();

  // use states
  const [sorting, setSorting] = useState<SortingState>([]);
  const [filters, setFilters] = useState<getCustomerSchema>({
    first: 0,
    rows: 10,
  });
  const [columnVisibility, setColumnVisibility] = useState<VisibilityState>({});
  const [rowSelection, setRowSelection] = useState({});
  const [selectedItem, setSelectedItem] = React.useState({});
  const [title, setTitle] = React.useState('');
  const [type, setType] = React.useState('');
  const [isModal, setIsModal] = React.useState(false);

  // APi
  const { data, refetch, isLoading } = trpc.coupon.get.useQuery(filters, {
    refetchOnWindowFocus: false,
  });

  const categoryData = React.useMemo(() => {
    return Array.isArray(data?.data) ? data?.data : [];
  }, [data]);

  // handle modal
  const handleEnbled = (data: any, type: string) => {
    if (!data?.is_approved) {
      setSelectedItem(data);
      setTitle('Coupon');
      setType(type);
      setIsModal(true);
    } else {
      toast({
        variant: 'success',
        title: `Customer is Already Approved!`,
      });
    }
  };
  // columns
  const columns: ColumnDef<Category>[] = [
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
      accessorKey: 'coupon_code',
      header: 'Coupon Code',
      cell: ({ row }) => (
        <div className="w-24 capitalize text-ellipsis whitespace-nowrap ">
          {row.getValue('coupon_code')}
        </div>
      ),
    },
    {
      accessorKey: 'discount',
      header: 'Discount',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {(row?.original?.discount).toFixed(2)}{' '}
          <sub>{row?.original?.is_percentage ? '%' : 'AED'}</sub>
        </div>
      ),
    },
    {
      accessorKey: 'is_percentage',
      header: 'Type',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.is_percentage ? 'percentage' : 'fixed'}
        </div>
      ),
    },
    {
      accessorKey: 'is_limited',
      header: 'Limit',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {row?.original?.is_limited
            ? row?.original?.coupon_limit
            : 'unlimited'}
        </div>
      ),
    },
    {
      id: 'is_approved',
      header: 'Enabled',

      cell: ({ row }) => {
        return (
          <div>
            <Switch
              checked={row?.original?.is_enabled}
              onCheckedChange={() => handleEnbled(row?.original, 'enabled')}
            />
          </div>
        );
      },
    },
    {
      accessorKey: 'start_date',
      header: 'Start Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {displayDate(row?.original?.start_date)}
        </div>
      ),
    },
    {
      accessorKey: 'end_date',
      header: 'End Date',
      cell: ({ row }) => (
        <div className="capitalize text-ellipsis whitespace-nowrap ">
          {displayDate(row?.original?.end_date)}
        </div>
      ),
    },
    {
      id: 'actions',
      enableHiding: false,
      cell: ({ row }) => {
        return (
          <DropdownMenu>
            <DropdownMenuTrigger asChild>
              <Button variant="ghost" className="h-8 w-8 p-0">
                <span className="sr-only">Open menu</span>
                <MoreHorizontal className="h-4 w-4" />
              </Button>
            </DropdownMenuTrigger>
            <DropdownMenuContent align="end">
              <DropdownMenuLabel>Actions</DropdownMenuLabel>
              <DropdownMenuSeparator />
              <Link href={`/admin/coupons/edit/${row?.original?.id}`}>
                <DropdownMenuItem>Edit Coupon</DropdownMenuItem>
              </Link>
            </DropdownMenuContent>
          </DropdownMenu>
        );
      },
    },
  ];

  const table = useReactTable({
    data: categoryData as Category[],
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

  function handlePagination(page: number) {
    if (page < 0) return;
    setFilters((prevFilters) => ({ ...prevFilters, first: page }));
  }

  return (
    <div className="w-full space-y-4">
      <div className="flex items-center justify-end gap-2">
        <DropdownMenu>
          <DropdownMenuTrigger asChild>
            <Button variant="outline">
              Columns <ChevronDown className="ml-2 h-4 w-4" />
            </Button>
          </DropdownMenuTrigger>
          <DropdownMenuContent align="end">
            {table
              .getAllColumns()
              .filter((column) => column.getCanHide())
              .map((column) => {
                return (
                  <DropdownMenuCheckboxItem
                    key={column.id}
                    className="capitalize"
                    checked={column.getIsVisible()}
                    onCheckedChange={(value) =>
                      column.toggleVisibility(!!value)
                    }
                  >
                    {column.id}
                  </DropdownMenuCheckboxItem>
                );
              })}
          </DropdownMenuContent>
        </DropdownMenu>
      </div>
      <div className="rounded-md border border-border ">
        <ScrollArea className="w-full ">
          <ScrollBar orientation="horizontal"></ScrollBar>

          <Table className="w-full overflow-x-scroll">
            <TableHeader>
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
      <div className="flex items-center justify-end space-x-2 py-4">
        <div className="space-x-2">
          <Button
            variant="outline"
            onClick={() => handlePagination(filters.first - 1)}
            disabled={filters.first === 0}
          >
            Previous
          </Button>
          <Button
            variant="outline"
            onClick={() => handlePagination(filters.first + 1)}
            disabled={(filters.first + 1) * filters.rows > (data?.count || 0)}
          >
            Next
          </Button>
        </div>
      </div>
      <CouponDialog
        selectedItem={selectedItem}
        setSelectedItem={setSelectedItem}
        title={title}
        setTitle={setTitle}
        isModal={isModal}
        setIsModal={setIsModal}
        refetch={refetch}
        type={type}
        setType={setType}
      />
      <LoadingDialog open={isLoading} text={'Loading data...'} />
    </div>
  );
}
